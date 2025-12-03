const SUPABASE_URL = 'https://yqunyiyvwyopxqickrhu.supabase.co';
const SUPABASE_KEY = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InlxdW55aXl2d3lvcHhxaWNrcmh1Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NjE3Mjc1NjgsImV4cCI6MjA3NzMwMzU2OH0.oFZj_Xs54U66cptthEboDmZzzn_Lpl0fGybVd5Xckb8';

const commentTableByMode = {
  Discipline: 'Referee',
  Scrum: 'Referee',
  Lineout: 'Referee',
  Rucks: 'Rucks'
};

function getCommentTableForMode(mode) {
  if (mode && commentTableByMode[mode]) {
    return commentTableByMode[mode];
  }
  return 'EIP_Carries';
}

const state = {
  analysisMode: 'Discipline',
  carryData: [],
  rucksData: [],
  refereeData: [],
  currentUser: 'Unknown user',
  isPlotInitialised: false,
  carryCache: null,
  commentState: {
    clipId: null,
    rowIdCol: null,
    commentCol: null,
    comments: [],
    tableName: getCommentTableForMode('Discipline'),
    rowData: null
  },
  globalComments: {
    entries: new Map(),
    rowIdCol: 'clip_id',
    commentCol: 'comment',
    isLoaded: false,
    isLoading: false
  },
  currentView: 'dashboard',
  team: {
    members: [],
    isLoaded: false,
    isLoading: false
  }
};

const selectors = {};
const pitchOnlyModes = new Set(['Scrum', 'Lineout', 'Rucks']);
const refereeModeEventTypes = {
  Discipline: ['Pen Won', 'Pen Conceded'],
  Scrum: ['Scrum'],
  Lineout: ['Lineout']
};

async function supabaseSelect(tableName, select = '*') {
  const url = `${SUPABASE_URL}/rest/v1/${tableName}?select=${encodeURIComponent(select)}`;
  const pageSize = 1000;
  const results = [];
  let from = 0;

  while (true) {
    const rangeHeader = `${from}-${from + pageSize - 1}`;
    const res = await fetch(url, {
      headers: {
        apikey: SUPABASE_KEY,
        Authorization: `Bearer ${SUPABASE_KEY}`,
        'Range-Unit': 'items',
        Range: rangeHeader
      }
    });

    if (!res.ok) {
      throw new Error(`Failed to fetch ${tableName}: ${res.status} ${res.statusText}`);
    }

    const text = await res.text();
    if (!text || text === '[]') {
      break;
    }

    let payload;
    try {
      payload = JSON.parse(text);
    } catch (err) {
      console.warn(`Unable to parse response for ${tableName}`, err);
      break;
    }

    if (!Array.isArray(payload) || payload.length === 0) {
      break;
    }

    results.push(...payload);

    const contentRange = res.headers.get('content-range');
    if (payload.length < pageSize || !contentRange) {
      break;
    }

    const match = contentRange.match(/^(?:\w+\s)?(\d+)-(\d+)\/(\d+|\*)$/);
    if (match) {
      const end = Number(match[2]);
      const total = match[3] === '*' ? null : Number(match[3]);
      if (Number.isFinite(total) && end >= total - 1) {
        break;
      }
      from = end + 1;
    } else {
      if (payload.length < pageSize) {
        break;
      }
      from += pageSize;
    }
  }

  return results;
}

async function supabaseUpdate(tableName, data, rowId, idCol = 'id') {
  const url = `${SUPABASE_URL}/rest/v1/${tableName}?${encodeURIComponent(idCol)}=eq.${encodeURIComponent(String(rowId))}`;
  const res = await fetch(url, {
    method: 'PATCH',
    headers: {
      apikey: SUPABASE_KEY,
      Authorization: `Bearer ${SUPABASE_KEY}`,
      'Content-Type': 'application/json',
      Prefer: 'return=minimal'
    },
    body: JSON.stringify(data)
  });

  if (!res.ok) {
    throw new Error(`Supabase update failed: ${res.status} ${await res.text()}`);
  }
}

async function supabaseInsert(tableName, data) {
  const url = `${SUPABASE_URL}/rest/v1/${tableName}`;
  const res = await fetch(url, {
    method: 'POST',
    headers: {
      apikey: SUPABASE_KEY,
      Authorization: `Bearer ${SUPABASE_KEY}`,
      'Content-Type': 'application/json',
      Prefer: 'return=representation'
    },
    body: JSON.stringify(data)
  });

  if (!res.ok) {
    throw new Error(`Supabase insert failed: ${res.status} ${await res.text()}`);
  }

  return res.json();
}

async function supabaseUpsert(tableName, data, onConflict = 'id') {
  const url = `${SUPABASE_URL}/rest/v1/${tableName}?on_conflict=${encodeURIComponent(onConflict)}`;
  const res = await fetch(url, {
    method: 'POST',
    headers: {
      apikey: SUPABASE_KEY,
      Authorization: `Bearer ${SUPABASE_KEY}`,
      'Content-Type': 'application/json',
      Prefer: 'resolution=merge-duplicates,return=representation'
    },
    body: JSON.stringify(data)
  });

  if (!res.ok) {
    throw new Error(`Supabase upsert failed: ${res.status} ${await res.text()}`);
  }

  return res.json();
}

function collectColumns(data) {
  const columns = new Set();
  data.forEach((row) => {
    if (row && typeof row === 'object') {
      Object.keys(row).forEach((key) => columns.add(key));
    }
  });
  return columns;
}

function findColumn(data, candidates) {
  if (!Array.isArray(data) || data.length === 0) return null;
  const columns = collectColumns(data);
  for (const name of candidates) {
    if (columns.has(name)) return name;
  }
  return null;
}

function getStorage() {
  try {
    return window.localStorage;
  } catch (error) {
    console.warn('Local storage unavailable', error);
    return null;
  }
}

function getCurrentUser() {
  const storage = getStorage();
  if (!storage) return state.currentUser;
  const stored = storage.getItem('ei-current-user');
  return stored || state.currentUser;
}

function cloneRow(row) {
  if (!row || typeof row !== 'object') return null;
  try {
    return structuredClone(row);
  } catch (error) {
    try {
      return JSON.parse(JSON.stringify(row));
    } catch (err) {
      console.warn('Failed to clone row', err);
      return null;
    }
  }
}

function loadCarryCache() {
  if (state.carryCache) return state.carryCache;
  const storage = getStorage();
  if (!storage) {
    state.carryCache = { version: 1, tables: {} };
    return state.carryCache;
  }

  const raw = storage.getItem('ei-comment-cache');
  if (!raw) {
    state.carryCache = { version: 1, tables: {} };
    return state.carryCache;
  }

  try {
    const parsed = JSON.parse(raw);
    if (!parsed || typeof parsed !== 'object') {
      state.carryCache = { version: 1, tables: {} };
      return state.carryCache;
    }

    if (!parsed.tables || typeof parsed.tables !== 'object') {
      parsed.tables = {};
    }

    parsed.version = parsed.version || 1;
    state.carryCache = parsed;
    return state.carryCache;
  } catch (error) {
    console.warn('Failed to parse carry cache', error);
    state.carryCache = { version: 1, tables: {} };
    return state.carryCache;
  }
}

function saveCarryCache(cache) {
  const storage = getStorage();
  if (!storage) return;
  try {
    storage.setItem('ei-comment-cache', JSON.stringify(cache));
  } catch (error) {
    console.warn('Failed to persist carry cache', error);
  }
}

function ensureTableCache(tableName) {
  const cache = loadCarryCache();
  if (!cache.tables[tableName]) {
    cache.tables[tableName] = { entries: {}, rowIdCol: null, commentCol: null };
  }
  return cache.tables[tableName];
}

function normaliseCommentPayload(value) {
  if (value === null || value === undefined) return null;

  if (typeof value === 'string') {
    const trimmed = value.trim();
    if (!trimmed) return null;
    return trimmed;
  }

  try {
    return JSON.stringify(value);
  } catch (error) {
    console.warn('Unable to serialise comment payload', error);
    return null;
  }
}

function cacheCommentEntry(tableName, clipId, rowIdCol, commentCol, row, payload, comments) {
  if (!tableName || !clipId) return;
  const payloadValue = normaliseCommentPayload(payload);
  const commentList = Array.isArray(comments) ? comments.map((entry) => ({ ...entry })) : [];
  const tableCache = ensureTableCache(tableName);
  const existing = tableCache.entries[String(clipId)] || {};

  if (!payloadValue && commentList.length === 0) {
    delete tableCache.entries[String(clipId)];
    saveCarryCache(state.carryCache);
    return;
  }

  const entryRow = cloneRow(row) || existing.row || null;

  tableCache.rowIdCol = rowIdCol || tableCache.rowIdCol || existing.rowIdCol || null;
  tableCache.commentCol = commentCol || tableCache.commentCol || existing.commentCol || null;

  tableCache.entries[String(clipId)] = {
    clipId: String(clipId),
    rowIdCol: rowIdCol || existing.rowIdCol || null,
    commentCol: commentCol || existing.commentCol || null,
    row: entryRow,
    payload: payloadValue,
    comments: commentList
  };

  saveCarryCache(state.carryCache);
}

function extractCommentDetails(row, commentCol) {
  if (!row || !commentCol) return null;
  const rawValue = row?.[commentCol];
  const payload = normaliseCommentPayload(rawValue);
  if (!payload) return null;
  const comments = parseCommentEntries(rawValue);
  if (!comments || comments.length === 0) return null;
  return { payload, comments };
}

function mergeDataWithCache(tableName, data) {
  const rows = Array.isArray(data) ? data.map((item) => ({ ...item })) : [];
  const cache = loadCarryCache();
  const tableCache = ensureTableCache(tableName);

  const candidateIdCols = ['data_row_id', 'id', 'row_id', 'ID'];
  const candidateCommentCols = ['comments', 'Comments', 'comment', 'Comment', 'conversation', 'Conversation'];

  const rowIdCol = findColumn(rows, candidateIdCols) || tableCache.rowIdCol || candidateIdCols.find((candidate) => candidate in (rows[0] || {})) || null;
  const commentCol = findColumn(rows, candidateCommentCols) || tableCache.commentCol || candidateCommentCols.find((candidate) => candidate in (rows[0] || {})) || null;

  tableCache.rowIdCol = rowIdCol;
  tableCache.commentCol = commentCol;

  const entryMap = tableCache.entries;
  const seen = new Set();

  rows.forEach((row) => {
    if (!row || typeof row !== 'object') return;
    const clipId = rowIdCol ? String(normaliseValue(row?.[rowIdCol])) : null;
    if (!clipId) return;
    seen.add(clipId);

    const commentDetails = extractCommentDetails(row, commentCol);
    if (commentDetails) {
      cacheCommentEntry(tableName, clipId, rowIdCol, commentCol, row, commentDetails.payload, commentDetails.comments);
      return;
    }

    const cached = entryMap[clipId];
    if (cached) {
      if (commentCol && cached.payload) {
        row[commentCol] = cached.payload;
      }
      cacheCommentEntry(tableName, clipId, rowIdCol, commentCol, row, cached.payload, cached.comments);
    }
  });

  Object.keys(entryMap).forEach((clipId) => {
    if (seen.has(clipId)) return;
    const cached = entryMap[clipId];
    if (!cached || !cached.row) return;
    const clone = { ...cached.row };
    if (rowIdCol && !clone[rowIdCol]) {
      const cachedValue = cached.rowIdCol ? cached.row[cached.rowIdCol] : null;
      if (cachedValue !== undefined && cachedValue !== null) {
        clone[rowIdCol] = cachedValue;
      } else {
        clone[rowIdCol] = cached.clipId;
      }
    }
    if (commentCol && cached.payload) {
      clone[commentCol] = cached.payload;
    }
    rows.push(clone);
  });

  saveCarryCache(cache);
  return rows;
}

function normaliseValue(value) {
  if (value === null || value === undefined) return null;
  if (Array.isArray(value)) {
    if (value.length === 0) return null;
    if (value.length === 1) return normaliseValue(value[0]);
  }
  if (typeof value === 'object') {
    if ('value' in value && Object.keys(value).length === 1) {
      return normaliseValue(value.value);
    }
  }
  return value;
}

function ensureGlobalCommentEntries() {
  if (!state.globalComments.entries) {
    state.globalComments.entries = new Map();
  }
  return state.globalComments.entries;
}

function getGlobalCommentEntry(clipId) {
  if (clipId === null || clipId === undefined) return null;
  const normalised = normaliseValue(clipId);
  if (normalised === null || normalised === undefined || normalised === '') {
    return null;
  }
  const entries = state.globalComments.entries;
  if (!entries || typeof entries.get !== 'function') return null;
  return entries.get(String(normalised)) || null;
}

function updateGlobalCommentCache(clipId, payload, comments) {
  if (clipId === null || clipId === undefined) return;
  const normalisedId = normaliseValue(clipId);
  if (normalisedId === null || normalisedId === undefined || normalisedId === '') {
    return;
  }
  const tableName = 'EIP_Data';
  const rowIdCol = state.globalComments.rowIdCol || 'clip_id';
  const commentCol = state.globalComments.commentCol || 'comments';
  const entries = ensureGlobalCommentEntries();
  const payloadValue = normaliseCommentPayload(payload);
  const cleanComments = Array.isArray(comments) ? comments.map((entry) => ({ ...entry })) : [];
  entries.set(String(normalisedId), { payload: payloadValue, comments: cleanComments });

  cacheCommentEntry(
    tableName,
    String(normalisedId),
    rowIdCol,
    commentCol,
    { [rowIdCol]: normalisedId, [commentCol]: payloadValue },
    payloadValue,
    cleanComments
  );
}

function applyGlobalCommentsToRows(tableName, rows) {
  if (!Array.isArray(rows) || rows.length === 0) return rows;
  const entries = state.globalComments.entries;
  if (!entries || entries.size === 0) return rows;

  const candidateIdCols = [
    'data_row_id',
    'id',
    'row_id',
    'ID',
    'Clip_ID',
    'clip_id',
    'Ruck_ID',
    'ruck_id',
    'Carry_ID',
    'carry_id'
  ];
  const candidateCommentCols = ['comments', 'Comments', 'comment', 'Comment', 'conversation', 'Conversation'];

  const rowIdCol = findColumn(rows, candidateIdCols);
  if (!rowIdCol) return rows;
  const commentCol = findColumn(rows, candidateCommentCols) || 'comments';

  rows.forEach((row) => {
    if (!row || typeof row !== 'object') return;
    const clipId = normaliseValue(row?.[rowIdCol]);
    if (clipId === null || clipId === undefined || clipId === '') return;
    const entry = entries.get(String(clipId));
    if (!entry || !entry.payload) return;
    row[commentCol] = entry.payload;
    cacheCommentEntry(tableName, clipId, rowIdCol, commentCol, row, entry.payload, entry.comments);
  });

  return rows;
}

function syncGlobalCommentsToDatasets() {
  applyGlobalCommentsToRows('EIP_Carries', state.carryData);
  applyGlobalCommentsToRows('Referee', state.refereeData);
  applyGlobalCommentsToRows('Rucks', state.rucksData);
}

function uniqueValues(data, column) {
  const values = new Set();
  data.forEach((row) => {
    const value = normaliseValue(row?.[column]);
    if (value !== null && value !== undefined && value !== '') {
      values.add(String(value));
    }
  });
  return Array.from(values);
}

function populateSelect(selectEl, values, placeholder = 'Select...', includePlaceholder = false) {
  const previous = selectEl.value;
  selectEl.innerHTML = '';

  if (!values || values.length === 0) {
    const option = document.createElement('option');
    option.value = '';
    option.textContent = placeholder;
    selectEl.append(option);
    selectEl.value = '';
    return;
  }

  if (includePlaceholder) {
    const option = document.createElement('option');
    option.value = '';
    option.textContent = placeholder;
    selectEl.append(option);
  }

  values.forEach((value) => {
    const option = document.createElement('option');
    option.value = value;
    option.textContent = value;
    selectEl.append(option);
  });

  if (previous && values.includes(previous)) {
    selectEl.value = previous;
  } else if (includePlaceholder) {
    selectEl.value = '';
  }
}

function setSpinnerVisible(visible) {
  const spinner = document.getElementById('plot-spinner');
  if (spinner) {
    spinner.classList.toggle('visible', visible);
  }
}

function updateSelectorsVisibility() {
  const isDiscipline = state.analysisMode === 'Discipline';
  const isPitchOnlyMode = pitchOnlyModes.has(state.analysisMode);
  const isRucksMode = state.analysisMode === 'Rucks';
  const showCarrySelectors = !isDiscipline && (!isPitchOnlyMode || isRucksMode);
  const matchDisplay = showCarrySelectors ? 'flex' : 'none';
  const otherDisplay = showCarrySelectors && !isRucksMode ? 'flex' : 'none';

  document.getElementById('match-wrapper').style.display = matchDisplay;
  document.getElementById('id-wrapper').style.display = otherDisplay;
  document.getElementById('half-wrapper').style.display = otherDisplay;

  if (selectors.rucksHint) {
    selectors.rucksHint.classList.toggle('hidden', !isRucksMode);
  }

  updateModeRefereeVisibility();
}

function populateRefereeSelectors() {
  if (!selectors.modeReferees) return;
  const refereeCol = findColumn(state.refereeData, ['Referee', 'referee', 'Official', 'official']);
  const selects = Object.values(selectors.modeReferees);
  if (selects.length === 0) return;

  if (!refereeCol) {
    selects.forEach((select) => populateSelect(select, [], 'No referees available', true));
    return;
  }

  const refs = uniqueValues(state.refereeData, refereeCol).sort((a, b) => a.localeCompare(b));
  selects.forEach((select) => populateSelect(select, refs, 'All referees', true));
}

function getActiveRefereeSelect() {
  if (!selectors.modeReferees) return null;
  return selectors.modeReferees[state.analysisMode] || null;
}

function getSelectedRefereeValue() {
  const select = getActiveRefereeSelect();
  return select ? select.value : '';
}

function updateModeRefereeVisibility() {
  if (!selectors.modeRefereeWrappers) return;
  selectors.modeRefereeWrappers.forEach((wrapper) => {
    const mode = wrapper.dataset.mode;
    wrapper.classList.toggle('active', mode === state.analysisMode);
  });
}

function populateMatchSelector() {
  if (state.analysisMode === 'Rucks') {
    const matchCol = findColumn(state.rucksData, ['Match', 'match', 'Fixture', 'fixture', 'Game', 'game']);
    if (!matchCol) {
      populateSelect(selectors.match, [], 'No matches available');
      return;
    }
    const matches = uniqueValues(state.rucksData, matchCol);
    populateSelect(selectors.match, matches, 'All matches', true);
    return;
  }

  const matchCol = findColumn(state.carryData, ['Opponent', 'opponent', 'Match', 'match', 'Fixture', 'fixture', 'Game', 'game']);
  if (!matchCol) {
    populateSelect(selectors.match, []);
    return;
  }
  const matches = uniqueValues(state.carryData, matchCol);
  populateSelect(selectors.match, matches, 'Select match');
}

function populateIdSelector() {
  if (state.analysisMode === 'Rucks') {
    populateSelect(selectors.id, []);
    return;
  }

  const idCol = findColumn(state.carryData, ['ID', 'id', 'Player_ID', 'player_id', 'Player', 'player', 'playerId', 'PlayerId']);
  if (!idCol) {
    populateSelect(selectors.id, []);
    return;
  }

  const matchCol = findColumn(state.carryData, ['Opponent', 'opponent', 'Match', 'match', 'Fixture', 'fixture', 'Game', 'game']);
  const matchValue = selectors.match.value;

  let filtered = state.carryData;
  if (matchCol && matchValue) {
    filtered = filtered.filter((row) => String(normaliseValue(row?.[matchCol])) === matchValue);
  }

  const ids = uniqueValues(filtered, idCol);
  populateSelect(selectors.id, ids, 'Select ID');
}

function populateHalfSelector() {
  if (state.analysisMode === 'Rucks') {
    populateSelect(selectors.half, []);
    return;
  }

  const halfCol = findColumn(state.carryData, ['Half', 'half', 'Period', 'period']);
  if (!halfCol) {
    populateSelect(selectors.half, []);
    return;
  }

  const matchCol = findColumn(state.carryData, ['Opponent', 'opponent', 'Match', 'match', 'Fixture', 'fixture', 'Game', 'game']);
  const idCol = findColumn(state.carryData, ['ID', 'id', 'Player_ID', 'player_id', 'Player', 'player', 'playerId', 'PlayerId']);
  const matchValue = selectors.match.value;
  const idValue = selectors.id.value;

  let filtered = state.carryData;
  if (matchCol && matchValue) {
    filtered = filtered.filter((row) => String(normaliseValue(row?.[matchCol])) === matchValue);
  }
  if (idCol && idValue) {
    filtered = filtered.filter((row) => String(normaliseValue(row?.[idCol])) === idValue);
  }

  const halves = uniqueValues(filtered, halfCol);
  populateSelect(selectors.half, halves, 'Select Half');
}

function buildPitchLayout() {
  const shapes = [];
  const addLine = (x0, y0, x1, y1, width = 2) => {
    shapes.push({
      type: 'line',
      x0,
      x1,
      y0,
      y1,
      line: { color: 'white', width },
      layer: 'below'
    });
  };

  shapes.push({
    type: 'rect',
    x0: 0,
    x1: 100,
    y0: 0,
    y1: 70,
    line: { width: 0 },
    fillcolor: 'forestgreen',
    layer: 'below'
  });

  addLine(0, 0, 0, 70);
  addLine(100, 0, 100, 70);
  addLine(0, 0, 100, 0);
  addLine(0, 70, 100, 70);

  [22, 50, 78].forEach((x) => addLine(x, 0, x, 70));
  [5, 95, 40, 60].forEach((x) => addLine(x, 2.5, x, 7.5));
  [5, 95, 40, 60].forEach((x) => addLine(x, 12.5, x, 17.5));
  [5, 95, 40, 60].forEach((x) => addLine(x, 22.5, x, 27.5));
  [5, 95, 40, 60].forEach((x) => addLine(x, 32.5, x, 37.5));
  [5, 95, 40, 60].forEach((x) => addLine(x, 42.5, x, 47.5));
  [5, 95, 40, 60].forEach((x) => addLine(x, 52.5, x, 57.5));
  [5, 95, 40, 60].forEach((x) => addLine(x, 62.5, x, 67.5));

  const channelSegments = [
    [5, 10],
    [19.5, 24.5],
    [37.5, 42.5],
    [47.5, 52.5],
    [57.5, 62.5],
    [75.5, 80.5],
    [90, 95]
  ];
  channelSegments.forEach(([start, end]) => {
    addLine(start, 5, end, 5);
    addLine(start, 65, end, 65);
    addLine(start, 15, end, 15);
    addLine(start, 55, end, 55);
  });

  const goalPosts = [
    { x: 0, y: 32.5 },
    { x: 0, y: 37.5 },
    { x: 100, y: 32.5 },
    { x: 100, y: 37.5 }
  ];

  const goalScatter = {
    type: 'scatter',
    mode: 'markers',
    x: goalPosts.map((p) => p.x),
    y: goalPosts.map((p) => p.y),
    marker: { color: 'white', size: 8 },
    hoverinfo: 'skip'
  };

  const layout = {
    shapes,
    xaxis: {
      range: [0, 100],
      showgrid: false,
      zeroline: false,
      showticklabels: false,
      fixedrange: true
    },
    yaxis: {
      range: [0, 70],
      showgrid: false,
      zeroline: false,
      showticklabels: false,
      scaleanchor: 'x',
      scaleratio: 1,
      fixedrange: true
    },
    plot_bgcolor: 'black',
    paper_bgcolor: 'black',
    margin: { l: 20, r: 20, t: 20, b: 20 },
    showlegend: false,
    hovermode: 'closest'
  };

  return { layout, baseData: [goalScatter] };
}

function buildDisciplineTraces(
  filtered,
  xCol,
  yCol,
  rowIdCol,
  offenceCol,
  options = {},
  tableName = 'Referee'
) {
  const traces = [];
  const { disableHover = false, markerColor } = options;
  const commentTable = tableName || 'Referee';
  const customData = filtered.map((row) => {
    const rowId = normaliseValue(row?.[rowIdCol]);
    return `${commentTable}|${String(rowId)}`;
  });
  const tooltip = filtered.map((row) => {
    const offence = offenceCol ? normaliseValue(row?.[offenceCol]) : null;
    return offence ? `<b>Offence:</b> ${offence}` : 'Offence information unavailable';
  });
  const hoverTemplate = disableHover ? '<extra></extra>' : '%{text}<extra></extra>';

  if (offenceCol) {
    const groups = new Map();
    filtered.forEach((row, index) => {
      const offence = normaliseValue(row?.[offenceCol]) || 'Unknown';
      if (!groups.has(offence)) groups.set(offence, []);
      groups.get(offence).push(index);
    });

    groups.forEach((indices, offence) => {
      const marker = {
        size: 12,
        line: { color: '#ffffff', width: 2 },
        opacity: 0.95,
        symbol: 'circle'
      };

      if (markerColor) {
        marker.color = markerColor;
      }

      traces.push({
        type: 'scatter',
        mode: 'markers',
        name: offence,
        x: indices.map((i) => Number(filtered[i]?.[xCol])),
        y: indices.map((i) => Number(filtered[i]?.[yCol])),
        text: disableHover ? [] : indices.map((i) => tooltip[i]),
        customdata: indices.map((i) => customData[i]),
        marker,
        hovertemplate: hoverTemplate
      });
    });
  } else {
    traces.push({
      type: 'scatter',
      mode: 'markers',
      name: 'Location',
      x: filtered.map((row) => Number(row?.[xCol])),
      y: filtered.map((row) => Number(row?.[yCol])),
      text: disableHover ? [] : tooltip,
      customdata: customData,
      marker: {
        size: 12,
        color: markerColor || '#ffcc00',
        line: { color: '#ffffff', width: 2 },
        opacity: 0.95,
        symbol: 'circle'
      },
      hovertemplate: hoverTemplate
    });
  }

  return traces;
}

function buildCarryTraces(filtered, startXCol, startYCol, endXCol, endYCol, rowIdCol) {
  const lines = {
    type: 'scatter',
    mode: 'lines',
    hoverinfo: 'skip',
    line: { color: 'white', width: 2 },
    x: [],
    y: []
  };

  const points = {
    type: 'scatter',
    mode: 'markers',
    marker: {
      color: '#ffcc00',
      size: 14,
      line: { color: '#ffffff', width: 2 },
      opacity: 0.95,
      symbol: 'circle'
    },
    x: [],
    y: [],
    text: [],
    customdata: [],
    hovertemplate: '%{text}<extra></extra>'
  };

  filtered.forEach((row) => {
    const startX = Number(row?.[startXCol]);
    const startY = Number(row?.[startYCol]);
    const endX = Number(row?.[endXCol]);
    const endY = Number(row?.[endYCol]);
    const clipId = normaliseValue(row?.[rowIdCol]);

    if (
      Number.isFinite(startX) &&
      Number.isFinite(startY) &&
      Number.isFinite(endX) &&
      Number.isFinite(endY) &&
      clipId !== null &&
      clipId !== ''
    ) {
      lines.x.push(startX, endX, null);
      lines.y.push(startY, endY, null);

      points.x.push(endX);
      points.y.push(endY);
      points.text.push(`<b>Carry ID:</b> ${clipId}<br><b>Start:</b> ${startX}, ${startY}`);
      points.customdata.push(`EIP_Carries|${clipId}`);
    }
  });

  return [lines, points];
}

function buildRuckTrace(rows, startXCol, startYCol, rowIdCol, matchCol) {
  const trace = {
    type: 'scatter',
    mode: 'markers',
    x: [],
    y: [],
    text: [],
    customdata: [],
    hovertemplate: '%{text}<extra></extra>',
    marker: {
      size: 12,
      color: '#00ffb3',
      line: { color: '#062d12', width: 1 }
    },
    name: 'Ruck Start'
  };

  const commentTable = getCommentTableForMode('Rucks');
  rows.forEach((row) => {
    const startX = Number(row?.[startXCol]);
    const startY = Number(row?.[startYCol]);
    const clipId = normaliseValue(row?.[rowIdCol]);
    if (!Number.isFinite(startX) || !Number.isFinite(startY) || clipId === null || clipId === '') {
      return;
    }

    const matchValue = matchCol ? normaliseValue(row?.[matchCol]) : '';
    trace.x.push(startX);
    trace.y.push(startY);
    trace.text.push(`${matchValue || 'Unknown Match'}`);
    trace.customdata.push(`${commentTable}|${clipId}`);
  });

  return trace;
}

function renderEmptyPitch() {
  const { layout, baseData } = buildPitchLayout();
  const plotElement = document.getElementById('carry-plot');
  plotWithInteractions(plotElement, baseData, layout);
}

function renderRefereeModePlot(plotElement, baseData, layout, allowedEventTypes = []) {
  const refereeCol = findColumn(state.refereeData, ['Referee', 'referee', 'Official', 'official']);
  const xCandidates = ['Start_X', 'start_x', 'X', 'x', 'Location_X', 'location_x', 'Start X', 'start x'];
  const yCandidates = ['Start_Y', 'start_y', 'Y', 'y', 'Location_Y', 'location_y', 'Start Y', 'start y'];
  const xCol = findColumn(state.refereeData, xCandidates);
  const yCol = findColumn(state.refereeData, yCandidates);
  const rowIdCol = findColumn(state.refereeData, ['data_row_id', 'id', 'row_id', 'ID', 'Clip_ID', 'clip_id', 'ClipId', 'clipId']);
  const offenceCol = findColumn(state.refereeData, ['Offence', 'offence', 'Offense', 'offense']);
  const eventTypeCol = findColumn(state.refereeData, ['Event_Type', 'event_type', 'EventType', 'eventtype']);

  if (!xCol || !yCol || !rowIdCol) {
    plotWithInteractions(plotElement, baseData, layout);
    return;
  }

  let filtered = state.refereeData.filter((row) => {
    const x = Number(row?.[xCol]);
    const y = Number(row?.[yCol]);
    const rowId = normaliseValue(row?.[rowIdCol]);
    return (
      Number.isFinite(x) &&
      Number.isFinite(y) &&
      rowId !== null &&
      rowId !== undefined &&
      rowId !== ''
    );
  });

  if (eventTypeCol && Array.isArray(allowedEventTypes) && allowedEventTypes.length > 0) {
    const allowed = allowedEventTypes.map((type) => String(type).toLowerCase());
    filtered = filtered.filter((row) => {
      const value = normaliseValue(row?.[eventTypeCol]);
      if (!value && value !== 0) return false;
      return allowed.includes(String(value).toLowerCase());
    });
  }

  const selectedRef = getSelectedRefereeValue();
  if (refereeCol && selectedRef) {
    filtered = filtered.filter((row) => String(normaliseValue(row?.[refereeCol])) === selectedRef);
  }

  if (filtered.length === 0) {
    plotWithInteractions(plotElement, baseData, layout);
    return;
  }

  const disableHover = state.analysisMode === 'Scrum' || state.analysisMode === 'Lineout';
  const commentTable = getCommentTableForMode(state.analysisMode);
  const traces = buildDisciplineTraces(filtered, xCol, yCol, rowIdCol, offenceCol, {
    disableHover,
    markerColor: disableHover ? '#9e9e9e' : undefined
  }, commentTable);
  const layoutWithLegend = {
    ...layout,
    showlegend: true,
    legend: {
      orientation: 'h',
      x: 0.5,
      xanchor: 'center',
      y: -0.1,
      yanchor: 'top',
      bgcolor: 'black',
      font: { color: 'white' }
    }
  };

  plotWithInteractions(plotElement, [...baseData, ...traces], layoutWithLegend);
}

function handlePlotClick(event) {
  const payload = event.points?.[0]?.customdata;
  if (!payload) return;

  const debugEl = document.getElementById('debug-click');
  if (debugEl) {
    debugEl.textContent = payload;
  }

  handleClipClick(payload);
}

function handlePitchSurfaceClick(plotElement, event) {
  if (!plotElement || !event) return;
  if (event.target.closest('.modebar') || event.target.closest('.legend')) return;
  if (event.target.closest('.point')) return;

  const coordinates = getPlotCoordinatesFromEvent(plotElement, event);
  if (!coordinates) return;

  startLocationCommentThread(state.analysisMode, coordinates);
}

function bindPlotInteractions(plotElement) {
  if (!plotElement) return;

  if (typeof plotElement.removeAllListeners === 'function') {
    plotElement.removeAllListeners('plotly_click');
  }

  plotElement.on('plotly_click', handlePlotClick);

  if (plotElement.__pitchClickHandler) {
    plotElement.removeEventListener('click', plotElement.__pitchClickHandler);
  }

  const backgroundHandler = (event) => handlePitchSurfaceClick(plotElement, event);
  plotElement.__pitchClickHandler = backgroundHandler;
  plotElement.addEventListener('click', backgroundHandler);
  state.isPlotInitialised = true;
}

function plotWithInteractions(plotElement, data, layout, config = { responsive: true }) {
  if (!plotElement) return;

  Plotly.newPlot(plotElement, data, layout, config)
    .then(() => {
      bindPlotInteractions(plotElement);
    })
    .catch((error) => {
      console.error('Failed to render plot', error);
    });
}

function renderRucksPlot(plotElement, baseData, layout) {
  const rows = Array.isArray(state.rucksData) ? state.rucksData : [];
  const startXCol = findColumn(rows, ['Start-X Location', 'Start_X', 'StartX', 'Start-X', 'Start X']);
  const startYCol = findColumn(rows, ['Start_Y Location', 'Start_Y', 'StartY', 'Start-Y', 'Start Y']);
  const rowIdCol = findColumn(rows, ['data_row_id', 'id', 'row_id', 'ID', 'Clip_ID', 'clip_id', 'ClipId']);

  if (!startXCol || !startYCol || !rowIdCol || rows.length === 0) {
    plotWithInteractions(plotElement, baseData, layout);
    return;
  }

  const matchCol = findColumn(rows, ['Match', 'match', 'Fixture', 'fixture', 'Game', 'game', 'Opponent', 'opponent']);
  const selectedMatch = selectors.match?.value;

  let filtered = rows.filter((row) => {
    const x = Number(row?.[startXCol]);
    const y = Number(row?.[startYCol]);
    const clipId = normaliseValue(row?.[rowIdCol]);
    return Number.isFinite(x) && Number.isFinite(y) && clipId !== null && clipId !== '';
  });

  if (matchCol && selectedMatch) {
    filtered = filtered.filter((row) => String(normaliseValue(row?.[matchCol])) === selectedMatch);
  }

  if (filtered.length === 0) {
    plotWithInteractions(plotElement, baseData, layout);
    return;
  }

  const trace = buildRuckTrace(filtered, startXCol, startYCol, rowIdCol, matchCol);
  if (!trace.x.length) {
    plotWithInteractions(plotElement, baseData, layout);
    return;
  }

  const layoutWithLegend = {
    ...layout,
    showlegend: true,
    legend: {
      orientation: 'h',
      x: 0.5,
      xanchor: 'center',
      y: -0.12,
      yanchor: 'top',
      bgcolor: 'rgba(0, 0, 0, 0.6)',
      font: { color: '#fff' }
    }
  };

  plotWithInteractions(plotElement, [...baseData, trace], layoutWithLegend);
}

function renderPlot() {
  const { layout, baseData } = buildPitchLayout();
  const plotElement = document.getElementById('carry-plot');
  const eventFilter = refereeModeEventTypes[state.analysisMode];

  if (Array.isArray(eventFilter)) {
    renderRefereeModePlot(plotElement, baseData, layout, eventFilter);
    return;
  }

  if (state.analysisMode === 'Rucks') {
    renderRucksPlot(plotElement, baseData, layout);
    return;
  }

  if (pitchOnlyModes.has(state.analysisMode)) {
    plotWithInteractions(plotElement, baseData, layout);
    return;
  }

  {
    const matchValue = selectors.match.value;
    const idValue = selectors.id.value;

    const opponentCol = findColumn(state.carryData, ['Opponent', 'opponent', 'Match', 'match']);
    const idCol = findColumn(state.carryData, ['ID', 'id', 'Player_ID', 'player_id']);
    const startXCol = findColumn(state.carryData, ['Start_X', 'start_x', 'StartX']);
    const startYCol = findColumn(state.carryData, ['Start_Y', 'start_y', 'StartY']);
    const endXCol = findColumn(state.carryData, ['End_X', 'end_x', 'EndX']);
    const endYCol = findColumn(state.carryData, ['End_Y', 'end_y', 'EndY']);
    const rowIdCol = findColumn(state.carryData, ['data_row_id', 'id', 'row_id', 'ID']);

    if (!opponentCol || !idCol || !startXCol || !startYCol || !endXCol || !endYCol || !rowIdCol) {
      plotWithInteractions(plotElement, baseData, layout);
      return;
    }

    if (!matchValue || !idValue) {
      plotWithInteractions(plotElement, baseData, layout);
      return;
    }

    const filtered = state.carryData.filter((row) =>
      String(normaliseValue(row?.[opponentCol])) === matchValue &&
      String(normaliseValue(row?.[idCol])) === idValue
    );

    if (filtered.length === 0) {
      plotWithInteractions(plotElement, baseData, layout);
      return;
    }

    const traces = buildCarryTraces(filtered, startXCol, startYCol, endXCol, endYCol, rowIdCol);
    plotWithInteractions(plotElement, [...baseData, ...traces], layout);
  }
}

function quantiseCoordinate(value, step = 0.5) {
  if (!Number.isFinite(value)) return null;
  const rounded = Math.round(value / step) * step;
  return Number(rounded.toFixed(2));
}

function buildLocationClipId(mode, x, y) {
  const key = `${mode || 'Unknown'}|${x}|${y}`;
  let h1 = 0x811c9dc5;
  let h2 = 0x9e3779b9;
  let h3 = 0x85ebca6b;
  let h4 = 0xc2b2ae35;

  for (let i = 0; i < key.length; i += 1) {
    const code = key.charCodeAt(i);
    h1 = ((h1 ^ code) * 0x01000193) >>> 0;
    h2 = (h2 + code + ((h2 << 6) >>> 0) + ((h2 >> 2) >>> 0)) >>> 0;
    h3 = ((h3 ^ (code * 17)) >>> 0);
    h4 = (h4 + ((code << 3) >>> 0) ^ ((h4 >>> 11) >>> 0)) >>> 0;
  }

  const hex = [h1, h2, h3, h4].map((num) => (num >>> 0).toString(16).padStart(8, '0')).join('').slice(0, 32);
  return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`;
}

function setChatPlaceholder(text) {
  const textarea = document.getElementById('chat-message');
  if (textarea) {
    textarea.placeholder = text;
  }
}

function getPlotCoordinatesFromEvent(plotElement, event) {
  if (!plotElement || !plotElement._fullLayout || !event) return null;
  const layoutSize = plotElement._fullLayout._size || {};
  const rect = plotElement.getBoundingClientRect();
  const xPixel = event.clientX - rect.left - (layoutSize.l || 0);
  const yPixel = event.clientY - rect.top - (layoutSize.t || 0);

  const xaxis = plotElement._fullLayout.xaxis;
  const yaxis = plotElement._fullLayout.yaxis;
  if (!xaxis || !yaxis || typeof xaxis.p2l !== 'function' || typeof yaxis.p2l !== 'function') {
    return null;
  }

  const xVal = xaxis.p2l(xPixel);
  const yVal = yaxis.p2l(yPixel);

  if (!Number.isFinite(xVal) || !Number.isFinite(yVal)) {
    return null;
  }

  const xRange = Array.isArray(xaxis.range) ? xaxis.range : [0, 100];
  const yRange = Array.isArray(yaxis.range) ? yaxis.range : [0, 70];
  const xMin = Math.min(xRange[0], xRange[1]);
  const xMax = Math.max(xRange[0], xRange[1]);
  const yMin = Math.min(yRange[0], yRange[1]);
  const yMax = Math.max(yRange[0], yRange[1]);

  if (xVal < xMin || xVal > xMax || yVal < yMin || yVal > yMax) {
    return null;
  }

  return { x: xVal, y: yVal };
}

function startLocationCommentThread(mode, coordinates) {
  const quantisedX = quantiseCoordinate(coordinates?.x);
  const quantisedY = quantiseCoordinate(coordinates?.y);
  if (quantisedX === null || quantisedY === null) return;

  const clipId = buildLocationClipId(mode, quantisedX, quantisedY);
  const existing = getGlobalCommentEntry(clipId);
  const baseComments = existing?.comments ? existing.comments.map((entry) => ({ ...entry })) : [];

  state.commentState = {
    clipId,
    rowIdCol: 'clip_id',
    commentCol: 'comment',
    comments: baseComments,
    tableName: 'Comments',
    rowData: { x: quantisedX, y: quantisedY, mode }
  };

  setChatPlaceholder(`Comment at (${quantisedX}, ${quantisedY})`);
  updateChatThread();
  loadCommentsForClip(clipId);
  openModal('');

  const debugEl = document.getElementById('debug-click');
  if (debugEl) {
    debugEl.textContent = `Pitch note | Mode: ${mode || 'Unknown'} | X: ${quantisedX}, Y: ${quantisedY} | clip_id: ${clipId}`;
  }
}

function parseCommentEntries(value) {
  if (value === null || value === undefined) return [];

  const normaliseEntry = (entry) => {
    if (!entry || typeof entry !== 'object') return null;
    const author = entry.author ?? entry.user ?? 'You';
    const message = entry.message ?? entry.text ?? '';
    const timestamp = entry.timestamp ?? entry.time ?? '';
    const align = entry.align;
    if (!message) return null;
    return {
      author: String(author || 'You'),
      message: String(message),
      timestamp: timestamp ? String(timestamp) : '',
      align: align === 'left' || align === 'right' ? align : undefined
    };
  };

  if (Array.isArray(value)) {
    const entries = value
      .map((item) => {
        if (Array.isArray(item)) {
          return parseCommentEntries(item);
        }
        if (typeof item === 'object' && !Array.isArray(item)) {
          return normaliseEntry(item);
        }
        if (typeof item === 'string') {
          return normaliseEntry({ message: item });
        }
        return null;
      })
      .flat()
      .filter(Boolean);

    if (entries.length > 0) return entries;
  }

  if (typeof value === 'object') {
    const values = Object.values(value);
    if (values.every((item) => typeof item === 'object')) {
      const entries = values
        .map((item) => (Array.isArray(item) ? parseCommentEntries(item) : normaliseEntry(item)))
        .flat()
        .filter(Boolean);
      if (entries.length > 0) return entries;
    }
    const entry = normaliseEntry(value);
    if (entry) return [entry];
  }

  if (typeof value === 'string') {
    const trimmed = value.trim();
    if (!trimmed) return [];
    try {
      const parsed = JSON.parse(trimmed);
      const entries = parseCommentEntries(parsed);
      if (entries.length > 0) return entries;
    } catch (err) {
      // fall through
    }
    return [
      {
        author: 'Unknown',
        message: trimmed,
        timestamp: '',
        align: 'left'
      }
    ];
  }

  return [];
}

function getCommentAlignment(entry) {
  if (entry?.align === 'left' || entry?.align === 'right') return entry.align;
  const author = entry?.author ? String(entry.author).trim().toLowerCase() : '';
  const current = state.currentUser ? String(state.currentUser).trim().toLowerCase() : '';
  if (author && current && author === current) return 'right';
  return 'left';
}

function normaliseCommentRow(row) {
  if (!row || typeof row !== 'object') return null;
  const message = row.comment ?? row.message;
  if (!message || String(message).trim() === '') return null;

  const createdDate = row.created_at || row.createdAt;

  return {
    author: row.username || row.user || 'Unknown',
    message: String(message),
    timestamp: createdDate ? new Date(createdDate).toLocaleString() : ''
  };
}

function generateCommentId() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  return `c-${Date.now()}-${Math.random().toString(16).slice(2, 10)}`;
}

async function loadCommentsForClip(clipId) {
  if (!clipId) return;
  const clipKey = String(clipId);

  const url = new URL(`${SUPABASE_URL}/rest/v1/Comments`);
  url.searchParams.set('select', '*');
  url.searchParams.set('clip_id', `eq.${clipKey}`);
  url.searchParams.append('order', 'created_at.asc');

  try {
    const res = await fetch(url.toString(), {
      headers: {
        apikey: SUPABASE_KEY,
        Authorization: `Bearer ${SUPABASE_KEY}`
      }
    });

    if (!res.ok) {
      throw new Error(`Failed to fetch comments: ${res.status} ${res.statusText}`);
    }

    const data = await res.json();
    if (state.commentState.clipId !== clipKey) return;

    const comments = Array.isArray(data)
      ? data.map((row) => normaliseCommentRow(row)).filter(Boolean)
      : [];

    state.commentState.comments = comments;
    ensureGlobalCommentEntries().set(String(clipKey), {
      payload: serialiseCommentThread(comments),
      comments
    });
    state.globalComments.rowIdCol = 'clip_id';
    state.globalComments.commentCol = 'comment';
    updateChatThread();
  } catch (error) {
    console.error('Failed to load comments for clip', error);
  }
}

function updateChatThread() {
  const container = document.getElementById('chat-thread');
  container.innerHTML = '';

  if (!state.commentState.comments || state.commentState.comments.length === 0) {
    const placeholder = document.createElement('div');
    placeholder.className = 'chat-placeholder';
    placeholder.textContent = 'No comments yet. Start the conversation.';
    container.append(placeholder);
    return;
  }

  state.commentState.comments.forEach((entry) => {
    const bubble = document.createElement('div');
    const alignment = getCommentAlignment(entry);
    bubble.className = `chat-bubble ${alignment}`;

    const message = document.createElement('div');
    message.className = 'chat-message';
    message.innerHTML = entry.message;

    bubble.append(message);

    const metaText = formatCommentMeta(entry.author, entry.timestamp);
    if (metaText) {
      const meta = document.createElement('div');
      meta.className = 'chat-meta';
      meta.textContent = metaText;
      bubble.append(meta);
    }

    container.append(bubble);
  });

  container.scrollTop = container.scrollHeight;
}

function formatCommentMeta(author, timestamp) {
  const cleanAuthor = author ? String(author).trim() : '';
  const cleanTimestamp = timestamp ? String(timestamp).trim() : '';
  if (!cleanAuthor && !cleanTimestamp) return '';
  if (!cleanAuthor) return cleanTimestamp;
  if (!cleanTimestamp) return cleanAuthor;
  return `${cleanAuthor} • ${cleanTimestamp}`;
}

function getTableAccessor(tableName) {
  switch (tableName) {
    case 'EIP_Carries':
      return {
        get: () => state.carryData,
        set: (data) => {
          state.carryData = data;
        },
        reload: loadCarryData
      };
    case 'Rucks':
      return {
        get: () => state.rucksData,
        set: (data) => {
          state.rucksData = data;
        },
        reload: loadRucksData
      };
    case 'Referee':
      return {
        get: () => state.refereeData,
        set: (data) => {
          state.refereeData = data;
        },
        reload: loadRefereeData
      };
    default:
      return null;
  }
}

function buildYouTubeEmbed(url, start, end) {
  if (!url || typeof url !== 'string') return '';
  const trimmed = url.trim();
  if (!trimmed) return '';

  let videoId = '';

  // youtu.be short links
  let match = trimmed.match(/youtu\.be\/([^?&/]+)/i);
  if (match) videoId = match[1];

  // youtube.com/watch?v=...
  if (!videoId) {
    match = trimmed.match(/[?&]v=([^&]+)/i);
    if (match) videoId = match[1];
  }

  // youtube.com/embed/...
  if (!videoId) {
    match = trimmed.match(/youtube\.com\/embed\/([^?&/]+)/i);
    if (match) videoId = match[1];
  }

  // youtube.com/shorts/...
  if (!videoId) {
    match = trimmed.match(/youtube\.com\/shorts\/([^?&/]+)/i);
    if (match) videoId = match[1];
  }

  // youtube.com/live/...
  if (!videoId) {
    match = trimmed.match(/youtube\.com\/live\/([^?&/]+)/i);
    if (match) videoId = match[1];
  }

  if (!videoId) return '';

  const params = [];
  const startNumber = Number(start);
  const endNumber = Number(end);
  if (Number.isFinite(startNumber) && startNumber >= 0) {
    params.push(`start=${Math.floor(startNumber)}`);
  }
  if (Number.isFinite(endNumber) && endNumber > 0) {
    params.push(`end=${Math.floor(endNumber)}`);
  }
  params.push('autoplay=1');

  return `https://www.youtube.com/embed/${videoId}${params.length ? `?${params.join('&')}` : ''}`;
}

function getUuidCandidate(row, rowIdCol, fallbackId) {
  const candidates = [
    rowIdCol ? row?.[rowIdCol] : null,
    row?.clip_id,
    row?.Clip_ID,
    row?.id,
    row?.ID,
    row?.data_row_id,
    fallbackId
  ];

  const normalised = candidates
    .map((value) => (value === undefined ? null : normaliseValue(value)))
    .find((value) => value !== null && value !== '');

  return normalised !== null && normalised !== undefined ? String(normalised) : null;
}

async function handleClipClick(payload) {
  if (!payload) return;

  const [tableName, clipId] = payload.split("|");
  if (!tableName || !clipId) return;

  // Load the row
  const rows = await supabaseSelect(
    tableName,
    "*"
  );

  const rowIdCol = findColumn(rows, ["data_row_id", "id", "ID", "row_id"]);
  const row = rows.find(r => String(r[rowIdCol]) === String(clipId));
  if (!row) {
    console.warn("Clip row not found for", clipId);
    return;
  }

  // Extract video fields
  const url = row.URL || row.youtube_url;
  const start = row.Clip_Start;
  const end = row.Clip_End;

  // Build YouTube URL with start/end
  let videoUrl = "";
  if (url) {
    const base = url.includes("watch") ? url : url.replace("youtu.be/", "www.youtube.com/watch?v=");
    videoUrl = `${base}&start=${start || 0}&end=${end || ""}`;
  }

  // Set iframe
  document.getElementById("modal-video").src = videoUrl;

  // Load comments into state
  const commentCol = findColumn([row], ["comments", "comment"]);
  const parsedComments = parseCommentEntries(row[commentCol] || "");

  state.commentState = {
    clipId,
    rowIdCol,
    commentCol,
    comments: parsedComments,
    tableName,
    rowData: row
  };

  updateChatThread();
  openModal(videoUrl);
}

function openModal(embedUrl) {
  const backdrop = document.getElementById('modal-backdrop');
  const iframe = document.getElementById('modal-video');
  iframe.src = '';
  if (embedUrl) {
    iframe.src = embedUrl;
  }
  backdrop.classList.remove('hidden');
  document.getElementById('chat-message').value = '';
  setTimeout(() => {
    const thread = document.getElementById('chat-thread');
    thread.scrollTop = thread.scrollHeight;
  }, 50);
}

function closeModal() {
  const backdrop = document.getElementById('modal-backdrop');
  const iframe = document.getElementById('modal-video');
  iframe.src = '';
  backdrop.classList.add('hidden');
}

function normaliseCommentEntry(entry) {
  if (!entry || typeof entry !== 'object') return null;

  const message = entry.message ?? entry.text ?? '';
  const cleanMessage = typeof message === 'string' ? message.trim() : String(message);
  if (!cleanMessage) return null;

  const author = entry.author ?? entry.user ?? state.currentUser ?? 'You';
  const timestamp = entry.timestamp ?? entry.time ?? '';
  const align = entry.align === 'left' || entry.align === 'right' ? entry.align : undefined;

  const normalised = {
    author: String(author || state.currentUser || 'You'),
    message: cleanMessage,
    timestamp: timestamp ? String(timestamp) : ''
  };

  const computedAlignment = align || getCommentAlignment({ ...entry, author: normalised.author });
  if (computedAlignment) {
    normalised.align = computedAlignment;
  }

  return normalised;
}

function serialiseCommentThread(comments) {
  if (!Array.isArray(comments)) return '[]';
  const normalised = comments
    .map((entry) => normaliseCommentEntry(entry))
    .filter(Boolean);
  return JSON.stringify(normalised);
}

async function loadGlobalComments() {
  if (state.globalComments.isLoading) return;
  state.globalComments.isLoading = true;
  try {
    const rows = await supabaseSelect('Comments', '*');
    const data = Array.isArray(rows) ? rows : [];

    const map = new Map();
    data.forEach((row) => {
      const clipId = normaliseValue(row?.clip_id);
      if (clipId === null || clipId === undefined || clipId === '') return;

      const entry = normaliseCommentRow(row);
      if (!entry) return;

      const key = String(clipId);
      const existing = map.get(key)?.comments || [];
      const comments = [...existing, entry];
      map.set(key, { payload: serialiseCommentThread(comments), comments });
    });

    state.globalComments.rowIdCol = 'clip_id';
    state.globalComments.commentCol = 'comment';

    state.globalComments.entries = map;
    syncGlobalCommentsToDatasets();
  } catch (error) {
    console.error('Failed to load global comments', error);
  } finally {
    state.globalComments.isLoading = false;
    state.globalComments.isLoaded = true;
  }
}

async function handleSendComment() {
  const textarea = document.getElementById('chat-message');
  const message = textarea.value.trim();

  if (!message) return;

  // We must have a clip_id available (UUID from EIP_Data.id)
  const clipId = state.commentState.clipId ? String(state.commentState.clipId) : '';
  if (!clipId) {
    console.error("❌ No clip_id available in state. Cannot send comment.");
    return;
  }

  const localTimestamp = new Date().toISOString();
  const localEntry = {
    author: state.currentUser || 'Unknown user',
    message,
    timestamp: new Date(localTimestamp).toLocaleString(),
    align: 'right'
  };

  try {
    const payload = {
      clip_id: clipId,
      username: state.currentUser || 'Unknown user',
      comment: message
    };

    const insertedRows = await supabaseInsert('Comments', payload);
    const insertedRow = Array.isArray(insertedRows) && insertedRows.length > 0 ? insertedRows[0] : null;
    const timestamp = insertedRow?.created_at || localTimestamp;

    const newEntry = {
      author: insertedRow?.username || state.currentUser || 'Unknown user',
      message: insertedRow?.comment || message,
      timestamp: new Date(timestamp).toLocaleString(),
      align: 'right'
    };

    state.commentState.comments.push(newEntry);

    const commentsPayload = serialiseCommentThread(state.commentState.comments);
    ensureGlobalCommentEntries().set(String(clipId), {
      payload: commentsPayload,
      comments: state.commentState.comments.map((entry) => ({ ...entry }))
    });
    state.globalComments.rowIdCol = 'clip_id';
    state.globalComments.commentCol = 'comment';

    updateChatThread();
    textarea.value = '';
  } catch (err) {
    console.error('❌ Unexpected error while sending comment:', err);
    // Fallback: still show locally so the user sees their message
    state.commentState.comments.push(localEntry);
    updateChatThread();
  }
}


async function loadCarryData() {
  try {
    const data = await supabaseSelect('EIP_Carries', '*');
    state.carryData = mergeDataWithCache('EIP_Carries', Array.isArray(data) ? data : []);
    applyGlobalCommentsToRows('EIP_Carries', state.carryData);
  } catch (error) {
    console.error('Failed to load carry data', error);
    state.carryData = mergeDataWithCache('EIP_Carries', []);
    applyGlobalCommentsToRows('EIP_Carries', state.carryData);
  }
}

async function loadRucksData() {
  try {
    const data = await supabaseSelect('Rucks', '*');
    state.rucksData = mergeDataWithCache('Rucks', Array.isArray(data) ? data : []);
    applyGlobalCommentsToRows('Rucks', state.rucksData);
  } catch (error) {
    console.error('Failed to load rucks data', error);
    state.rucksData = mergeDataWithCache('Rucks', []);
    applyGlobalCommentsToRows('Rucks', state.rucksData);
  }
}

async function loadRefereeData() {
  try {
    const data = await supabaseSelect('Referee', '*');
    state.refereeData = mergeDataWithCache('Referee', Array.isArray(data) ? data : []);
    applyGlobalCommentsToRows('Referee', state.refereeData);
  } catch (error) {
    console.error('Failed to load referee data', error);
    state.refereeData = mergeDataWithCache('Referee', []);
    applyGlobalCommentsToRows('Referee', state.refereeData);
  }
}

function setTeamEmptyMessage(message) {
  if (selectors.teamEmptyState) {
    selectors.teamEmptyState.textContent = message;
  }
}

function renderTeamMembers() {
  if (!selectors.teamTableBody) return;
  selectors.teamTableBody.innerHTML = '';
  const members = Array.isArray(state.team.members) ? state.team.members : [];

  if (members.length === 0) {
    if (selectors.teamEmptyState) {
      selectors.teamEmptyState.classList.remove('hidden');
    }
    return;
  }

  if (selectors.teamEmptyState) {
    selectors.teamEmptyState.classList.add('hidden');
  }

  members.forEach((member) => {
    const row = document.createElement('tr');
    ['forename', 'surname', 'role'].forEach((key) => {
      const cell = document.createElement('td');
      const value = member?.[key];
      cell.textContent = value ? String(value) : '';
      row.append(cell);
    });
    selectors.teamTableBody.append(row);
  });
}

async function loadTeamMembers(force = false) {
  if (state.team.isLoading) return;
  if (state.team.isLoaded && !force) {
    renderTeamMembers();
    return;
  }

  state.team.isLoading = true;
  setTeamEmptyMessage('Loading team members...');
  if (selectors.teamEmptyState) {
    selectors.teamEmptyState.classList.remove('hidden');
  }

  try {
    const data = await supabaseSelect('login_credentials', '*');
    state.team.members = Array.isArray(data) ? data : [];
    state.team.isLoaded = true;
    if (state.team.members.length === 0) {
      setTeamEmptyMessage('No team members have been added yet.');
    }
  } catch (error) {
    console.error('Failed to load login credentials', error);
    setTeamEmptyMessage('Unable to load team members.');
  } finally {
    state.team.isLoading = false;
    renderTeamMembers();
  }
}

function setUserModalSaving(isSaving) {
  if (!selectors.userModalSubmit) return;
  selectors.userModalSubmit.disabled = isSaving;
  selectors.userModalSubmit.textContent = isSaving ? 'Saving...' : 'Save User';
}

function openUserModal() {
  selectors.userModalBackdrop?.classList.remove('hidden');
}

function closeUserModal(resetForm = true) {
  selectors.userModalBackdrop?.classList.add('hidden');
  if (resetForm && selectors.addUserForm) {
    selectors.addUserForm.reset();
  }
  setUserModalSaving(false);
}

function getFormValue(formData, key) {
  const value = formData.get(key);
  return typeof value === 'string' ? value.trim() : '';
}

async function handleAddUserSubmit(event) {
  event.preventDefault();
  const form = event.target;
  const formData = new FormData(form);
  const payload = {
    forename: getFormValue(formData, 'forename'),
    surname: getFormValue(formData, 'surname'),
    email: getFormValue(formData, 'email'),
    username: getFormValue(formData, 'username'),
    password: getFormValue(formData, 'password'),
    role: getFormValue(formData, 'role')
  };

  const missing = Object.entries(payload).find(([, value]) => !value);
  if (missing) {
    alert('Please complete all fields before saving.');
    return;
  }

  setUserModalSaving(true);

  try {
    const inserted = await supabaseInsert('login_credentials', payload);
    const savedRow = Array.isArray(inserted) && inserted.length > 0 ? inserted[0] : payload;
    state.team.members = [savedRow, ...state.team.members];
    state.team.isLoaded = true;
    renderTeamMembers();
    form.reset();
    closeUserModal(false);
  } catch (error) {
    console.error('Failed to add user', error);
    alert('Unable to save the user right now. Please try again.');
  } finally {
    setUserModalSaving(false);
  }
}

function setSidebarActive(buttonId) {
  if (!selectors.sidebarButtons) return;
  selectors.sidebarButtons.forEach((button) => {
    button.classList.toggle('active', button.id === buttonId);
  });
}

function showView(view, buttonId = null) {
  state.currentView = view;
  if (view === 'manage-team') {
    selectors.dashboardView?.classList.add('hidden');
    selectors.manageTeamView?.classList.remove('hidden');
  } else {
    selectors.dashboardView?.classList.remove('hidden');
    selectors.manageTeamView?.classList.add('hidden');
  }

  if (buttonId) {
    setSidebarActive(buttonId);
  }

  document.body.classList.add('sidebar-collapse');
  document.body.classList.remove('sidebar-open');
}

function bindNavigationButtons() {
  const mapping = {
    'go-dashboard': 'dashboard',
    'go-analysis': 'dashboard',
    'go-manage-team': 'manage-team'
  };

  Object.entries(mapping).forEach(([buttonId, view]) => {
    const button = document.getElementById(buttonId);
    if (!button) return;
    button.addEventListener('click', async () => {
      if (view === 'manage-team') {
        await loadTeamMembers();
      }
      showView(view, buttonId);
    });
  });
}

function updateAiOutput() {
  const input = document.getElementById('ai-input');
  const output = document.getElementById('ai-output');
  const question = input.value.trim();
  output.textContent = `Mode: ${state.analysisMode} | You asked: ${question}`;
}

function bindSidebar() {
  const logo = document.getElementById('logo-click');
  const sidebar = document.getElementById('custom-sidebar');
  const body = document.body;

  function openSidebar() {
    body.classList.add('sidebar-open');
    body.classList.remove('sidebar-collapse');
  }

  function closeSidebar() {
    body.classList.add('sidebar-collapse');
    body.classList.remove('sidebar-open');
  }

  logo.addEventListener('click', (event) => {
    event.stopPropagation();
    if (body.classList.contains('sidebar-open')) {
      closeSidebar();
    } else {
      openSidebar();
    }
  });

  document.addEventListener('click', (event) => {
    if (
      body.classList.contains('sidebar-open') &&
      !sidebar.contains(event.target) &&
      event.target !== logo
    ) {
      closeSidebar();
    }
  });

  closeSidebar();
}

async function initialise() {
  selectors.analysisButtons = Array.from(document.querySelectorAll('.btn-group .btn'));
  selectors.match = document.getElementById('match-select');
  selectors.id = document.getElementById('id-select');
  selectors.half = document.getElementById('half-select');
  selectors.rucksHint = document.getElementById('rucks-hint');
  selectors.modeRefereeWrappers = Array.from(document.querySelectorAll('.mode-referee'));
  selectors.modeReferees = {};
  selectors.modeRefereeWrappers.forEach((wrapper) => {
    const mode = wrapper.dataset.mode;
    const select = wrapper.querySelector('select');
    if (!mode || !select) return;
    selectors.modeReferees[mode] = select;
    select.addEventListener('change', () => {
      if (state.analysisMode === mode) {
        renderPlot();
      }
    });
  });
  selectors.sidebarButtons = Array.from(document.querySelectorAll('.sidebar-btn'));
  selectors.dashboardView = document.getElementById('dashboard-view');
  selectors.manageTeamView = document.getElementById('manage-team-view');
  selectors.teamTableBody = document.getElementById('team-table-body');
  selectors.teamEmptyState = document.getElementById('team-empty-state');
  selectors.addUserBtn = document.getElementById('add-user-btn');
  selectors.userModalBackdrop = document.getElementById('user-modal-backdrop');
  selectors.userModalClose = document.getElementById('user-modal-close');
  selectors.addUserForm = document.getElementById('add-user-form');
  selectors.userModalSubmit = document.getElementById('user-modal-submit');

  state.currentUser = getCurrentUser();

  bindSidebar();
  bindNavigationButtons();
  showView('dashboard', 'go-dashboard');
  updateModeRefereeVisibility();

  selectors.analysisButtons.forEach((button) => {
    button.addEventListener('click', () => {
      selectors.analysisButtons.forEach((btn) => btn.classList.remove('active'));
      button.classList.add('active');
      state.analysisMode = button.dataset.mode;
      updateSelectorsVisibility();
      if (state.analysisMode === 'Rucks') {
        populateMatchSelector();
        populateIdSelector();
        populateHalfSelector();
      } else if (!pitchOnlyModes.has(state.analysisMode) && state.analysisMode !== 'Discipline') {
        populateMatchSelector();
        populateIdSelector();
        populateHalfSelector();
      }
      renderPlot();
      updateAiOutput();
    });
  });

  selectors.match.addEventListener('change', () => {
    if (state.analysisMode === 'Rucks') {
      renderPlot();
      return;
    }
    populateIdSelector();
    populateHalfSelector();
    renderPlot();
  });

  selectors.id.addEventListener('change', () => {
    if (state.analysisMode === 'Rucks') {
      return;
    }
    populateHalfSelector();
    renderPlot();
  });

  selectors.half.addEventListener('change', () => {
    if (state.analysisMode === 'Rucks') {
      return;
    }
    renderPlot();
  });

  document.getElementById('ai-input').addEventListener('input', updateAiOutput);

  selectors.addUserBtn?.addEventListener('click', openUserModal);
  selectors.userModalClose?.addEventListener('click', () => closeUserModal());
  selectors.userModalBackdrop?.addEventListener('click', (event) => {
    if (event.target.id === 'user-modal-backdrop') {
      closeUserModal();
    }
  });
  selectors.addUserForm?.addEventListener('submit', handleAddUserSubmit);

  document.getElementById('modal-close').addEventListener('click', closeModal);
  document.getElementById('modal-dismiss').addEventListener('click', closeModal);
  document.getElementById('modal-backdrop').addEventListener('click', (event) => {
    if (event.target.id === 'modal-backdrop') {
      closeModal();
    }
  });
  document.getElementById('chat-send').addEventListener('click', handleSendComment);

  setSpinnerVisible(true);
  try {
    await Promise.all([loadCarryData(), loadRefereeData(), loadRucksData(), loadGlobalComments()]);

    populateRefereeSelectors();

    if (state.analysisMode === 'Rucks') {
      populateMatchSelector();
      populateIdSelector();
      populateHalfSelector();
    } else if (!pitchOnlyModes.has(state.analysisMode) && state.analysisMode !== 'Discipline') {
      populateMatchSelector();
      populateIdSelector();
      populateHalfSelector();
    }

    renderPlot();
    updateSelectorsVisibility();
    updateAiOutput();
  } finally {
    setSpinnerVisible(false);
  }
}

document.addEventListener('DOMContentLoaded', initialise);
