# packages ----
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinydashboard)
library(fontawesome)
library(rsconnect)
library(ggplot2)
library(readr)
library(ggridges)
library(ggdist)
library(DT)
library(geomtextpath)
library(shinycssloaders)
library(lubridate)
library(plotly)
library(httr)
library(jsonlite)
library(config)
library(shinyjs)
library(bslib)
library(threejs)
library(later)


# USER CREDENTIALS ----
user_credentials <- data.frame(
  username = c("Billingham_Analyst", "Anselmians_Analyst",
               "HSFC_Analyst", "Hartlepool_Analyst", 
               "Burton_Analyst", "Test"),
  password = c("Performance2025", "Performance2026",
               "HSFCperformance", "OldBoys2025", 
               "BurtonPerformance2025", "Test"),
  stringsAsFactors = FALSE
)

# SUPABASE CONFIG ----
SUPABASE_URL <- "https://yqunyiyvwyopxqickrhu.supabase.co"
SUPABASE_KEY <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InlxdW55aXl2d3lvcHhxaWNrcmh1Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NjE3Mjc1NjgsImV4cCI6MjA3NzMwMzU2OH0.oFZj_Xs54U66cptthEboDmZzzn_Lpl0fGybVd5Xckb8"
SUPABASE_SERVICE_KEY <-"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InlxdW55aXl2d3lvcHhxaWNrcmh1Iiwicm9sZSI6InNlcnZpY2Vfcm9sZSIsImlhdCI6MTc2MTcyNzU2OCwiZXhwIjoyMDc3MzAzNTY4fQ._2lYCnTsTzNOhVbKkJI3vx1onan40BFx34Wsg6QV6QU"

supabase_insert <- function(table_name, data) {
  url <- paste0(SUPABASE_URL, "/rest/v1/", table_name)
  
  response <- POST(
    url,
    add_headers(
      "apikey" = SUPABASE_SERVICE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_SERVICE_KEY),
      "Content-Type" = "application/json",
      "Prefer" = "return=representation"
    ),
    body = toJSON(data, auto_unbox = TRUE)
  )
  
  return(response)
}

supabase_select <- function(table_name, select_clause = "*", order_by = NULL, filter = NULL) {
  url <- paste0(SUPABASE_URL, "/rest/v1/", table_name)
  query_params <- list(select = select_clause)
  if (!is.null(order_by)) query_params$order <- order_by
  if (!is.null(filter)) {
    for (key in names(filter)) {
      query_params[[key]] <- filter[[key]]
    }
  }
  response <- GET(
    url,
    add_headers(
      "apikey" = SUPABASE_SERVICE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_SERVICE_KEY)
    ),
    query = query_params
  )
  if (status_code(response) == 200) {
    raw_content <- content(response, "text", encoding = "UTF-8")
    if (!nzchar(raw_content)) {
      return(NULL)
    }
    
    data <- fromJSON(raw_content, flatten = TRUE)
    
    # Ensure the result is always a data frame for downstream logic
    if (is.data.frame(data)) {
      return(as.data.frame(data, stringsAsFactors = FALSE))
    } else if (is.list(data) && length(data) > 0) {
      return(as.data.frame(data, stringsAsFactors = FALSE))
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

# NEW: get EIP_Data for a match (both directions: Team=home/Opp=away OR Team=away/Opp=home)
supabase_select_eip_match <- function(home_team, away_team) {
  # first direction
  eip1 <- supabase_select(
    "EIP_Data",
    select_clause = "*",
    filter = list(
      Team = paste0("eq.", home_team),
      Opponent = paste0("eq.", away_team)
    )
  )
  # second direction (swapped)
  eip2 <- supabase_select(
    "EIP_Data",
    select_clause = "*",
    filter = list(
      Team = paste0("eq.", away_team),
      Opponent = paste0("eq.", home_team)
    )
  )
  # bind safely
  out <- NULL
  if (!is.null(eip1) && is.data.frame(eip1) && nrow(eip1) > 0) out <- eip1
  if (!is.null(eip2) && is.data.frame(eip2) && nrow(eip2) > 0) {
    if (is.null(out)) {
      out <- eip2
    } else {
      out <- bind_rows(out, eip2)
    }
  }
  out
}

# NEW: insert single analysis-row into EIP_Data
supabase_insert_eip_event <- function(row_list) {
  url <- paste0(SUPABASE_URL, "/rest/v1/EIP_Data")
  response <- POST(
    url,
    add_headers(
      "apikey" = SUPABASE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_KEY),
      "Content-Type" = "application/json",
      "Prefer" = "return=minimal"
    ),
    body = toJSON(row_list, auto_unbox = TRUE),
    encode = "raw"
  )
  
  # DEBUG OUTPUT
  cat("\n---- SUPABASE INSERT DEBUG ----\n")
  cat("Status:", status_code(response), "\n")
  cat("Response:\n")
  print(content(response, "text"))
  cat("--------------------------------\n\n")
  
  return(response)
}


# LOGIN UI ----

login_ui <- function() {
  div(
    style = 
      "height: 100vh; 
    display: flex; 
    flex-direction: column; 
    justify-content: center; 
    align-items: center; 
    background-image: url('https://raw.github.com/ConnorFewster/EI/main/EI-Performance%20-%20Gradient%20Backgrounds-Green-1.png');
    background-size: cover;
    background-position: center;
    position: relative;
    overflow: hidden;",
    
    # Three.js canvas container (background)
    div(
      id = "threejs-container",
      style = "
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      z-index: 1;"
    ),
    
    # Logo outside the box (above)
    div(
      style = "
      text-align: center;
      margin-bottom: 20px;
      position: relative;
      z-index: 2;",
      
      img(src = "https://raw.github.com/ConnorFewster/EI/main/EI%20Performance%20-%20Logo%20-%20Icon%20-%20Green.png",
          style = "
          height: 140px;
          width: auto;
          margin-bottom: 20px;")
    ),
    
    # Login form (foreground)
    div(
      style = "
      background: rgba(255, 255, 255, 0.1); 
      padding: 20px 10px;
      border-radius: 10px; 
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); 
      width: 400px;
      text-align: center;
      position: relative;
      z-index: 2;
      backdrop-filter: blur(10px);",
      
      # Custom floating label input fields
      div(
        style = "
        margin-bottom: 25px;
        position: relative;
        font-family:'Raleway', sans-serif;",
        
        # Username field with user icon
        div(
          style = "position: relative;",
          tags$input(
            id = "username",
            name = "username",
            type = "text",
            required = "",
            style = "
            width: 100%;
            padding: 12px 12px 12px 45px;
            border: 2px solid rgba(255, 255, 255, 0.3);
            border-radius: 8px;
            background: rgba(255, 255, 255, 0.1);
            color: white;
            font-size: 16px;
            font-family: 'Raleway', sans-serif;
            outline: none;
            transition: all 0.3s ease;
            box-sizing: border-box;",
            placeholder = " "
          ),
          tags$label(
            `for` = "username",
            style = "
            position: absolute;
            left: 45px;
            top: 50%;
            transform: translateY(-50%);
            color: rgba(255, 255, 255, 0.7);
            font-size: 16px;
            pointer-events: none;
            transition: all 0.3s ease;
            z-index: 2;",
            "Username"
          ),
          tags$div(
            style = "
            position: absolute;
            left: 15px;
            top: 50%;
            transform: translateY(-50%);
            color: rgba(255, 255, 255, 0.7);
            font-size: 18px;
            z-index: 3;",
            HTML("👤")
          )
        )
      ),
      
      div(
        style = "
        margin-bottom: 30px;
        position: relative;
        font-family:'Raleway', sans-serif;",
        
        # Password field with padlock icon
        div(
          style = "position: relative;",
          tags$input(
            id = "password",
            name = "password",
            type = "password",
            required = "",
            style = "
            width: 100%;
            padding: 12px 12px 12px 45px;
            border: 2px solid rgba(255, 255, 255, 0.3);
            border-radius: 8px;
            background: rgba(255, 255, 255, 0.1);
            color: white;
            font-size: 16px;
            font-family: 'Raleway', sans-serif;
            outline: none;
            transition: all 0.3s ease;
            box-sizing: border-box;",
            placeholder = " "
          ),
          tags$label(
            `for` = "password",
            style = "
            position: absolute;
            left: 45px;
            top: 50%;
            transform: translateY(-50%);
            color: rgba(255, 255, 255, 0.7);
            font-size: 16px;
            pointer-events: none;
            transition: all 0.3s ease;
            z-index: 2;",
            "Password"
          ),
          tags$div(
            style = "
            position: absolute;
            left: 15px;
            top: 50%;
            transform: translateY(-50%);
            color: rgba(255, 255, 255, 0.7);
            font-size: 18px;
            z-index: 3;",
            HTML("🔒")
          )
        )
      ),
      
      # Login form wrapped in form element for Enter key support
      tags$form(
        onsubmit = "return false;",
        style = "text-align: center; font-family: 'Raleway', sans-serif",
        actionButton("login", "Login", 
                     class = "btn-primary btn-lg",
                     type = "submit",
                     style = "
                     width: 100%; 
                     padding: 12px;
                     background-color: #218838;
                     color: white;
                     border: none;
                     cursor: pointer;")
      ),
      
      div(
        id = "login_error",
        style = "margin-top: 20px; text-align: center; color: #dc3545; font-weight: bold;",
        uiOutput("login_message")
      )
    ),
    
    # Sign up link below login box
    div(
      style = "
      margin-top: 20px;
      text-align: center;
      position: relative;
      z-index: 2;
      font-family: 'Raleway', sans-serif;",
      
      tags$span(
        style = "color: rgba(255, 255, 255, 0.8); font-size: 14px;",
        "Don't have an account? "
      ),
      tags$a(
        id = "signup_link",
        href = "?page=signup",
        target = "_blank",
        style = "
        color: #00ff88;
        font-size: 14px;
        font-weight: bold;
        text-decoration: none;
        transition: all 0.3s ease;
        cursor: pointer;",
        "Sign Up Now"
      )
    ),
    
    # Three.js script
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    
    # Custom CSS for input fields
    tags$style(HTML("
      input:focus + label,
      input:not(:placeholder-shown) + label {
        opacity: 0;
        visibility: hidden;
      }
      
      input:focus + label,
      input:valid + label {
        opacity: 0;
        visibility: hidden;
      }
      
      input:focus {
        border-color: #00ff88 !important;
        box-shadow: 0 0 10px rgba(0, 255, 136, 0.3);
      }
      
      input:focus + label + div {
        color: #00ff88 !important;
      }
      
      label {
        transition: opacity 0.3s ease, visibility 0.3s ease;
      }
      
      button:hover {
        background-color: #1e7e34 !important;
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
      }
      
      #signup_link:hover {
        color: #00ffaa !important;
        text-shadow: 0 0 10px rgba(0, 255, 136, 0.5);
        transform: scale(1.05);
      }
    ")),
    
    # JavaScript for Enter key functionality and signup link
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        const usernameInput = document.getElementById('username');
        const passwordInput = document.getElementById('password');
        const loginButton = document.getElementById('login');
        const signupLink = document.getElementById('signup_link');
        
        function handleEnterKey(event) {
          if (event.key === 'Enter') {
            event.preventDefault();
            if (loginButton) {
              loginButton.click();
            }
          }
        }
        
        if (usernameInput) {
          usernameInput.addEventListener('keypress', handleEnterKey);
        }
        if (passwordInput) {
          passwordInput.addEventListener('keypress', handleEnterKey);
        }
        
        const form = loginButton ? loginButton.closest('form') : null;
        if (form) {
          form.addEventListener('submit', function(event) {
            event.preventDefault();
            if (loginButton) {
              loginButton.click();
            }
          });
        }
      });
    ")),
    tags$script(HTML("
      function initThreeJS() {
        if (typeof THREE === 'undefined') {
          setTimeout(initThreeJS, 100);
          return;
        }
        
        const container = document.getElementById('threejs-container');
        if (!container) {
          return;
        }
        
        const scene = new THREE.Scene();
        const camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
        const renderer = new THREE.WebGLRenderer({ alpha: true, antialias: true });
        
        renderer.setSize(window.innerWidth, window.innerHeight);
        renderer.setClearColor(0x000000, 0);
        container.appendChild(renderer.domElement);
        
        const planetRadius = 6.5;
        const widthSegments = 16;
        const heightSegments = 12;
        const geometry = new THREE.SphereGeometry(planetRadius, widthSegments, heightSegments);
        const material = new THREE.MeshBasicMaterial({
          color: 0x808080,
          wireframe: true,
          transparent: true,
          opacity: 0.1
        });
        const planet = new THREE.Mesh(geometry, material);
        scene.add(planet);
        
        const nodeGeometry = new THREE.SphereGeometry(0.08, 8, 8);
        const nodeMaterial = new THREE.MeshBasicMaterial({ 
          color: 0x808080,
          transparent: true,
          opacity: 0.1
        });
        const nodes = [];
        
        for (let i = 0; i <= heightSegments; i++) {
          const theta = (i / heightSegments) * Math.PI;
          for (let j = 0; j <= widthSegments; j++) {
            const phi = (j / widthSegments) * Math.PI * 2;
            
            const node = new THREE.Mesh(nodeGeometry, nodeMaterial);
            
            node.position.x = planetRadius * Math.sin(theta) * Math.cos(phi);
            node.position.y = planetRadius * Math.cos(theta);
            node.position.z = planetRadius * Math.sin(theta) * Math.sin(phi);
            
            nodes.push(node);
            scene.add(node);
          }
        }
        
        const lineMaterial = new THREE.LineBasicMaterial({ 
          color: 0x808080,
          transparent: true, 
          opacity: 0.1
        });
        
        for (let i = 0; i < nodes.length; i++) {
          for (let j = i + 1; j < nodes.length; j++) {
            const distance = nodes[i].position.distanceTo(nodes[j].position);
            if (distance < 4.0 && Math.random() > 0.85) {
              const lineGeometry = new THREE.BufferGeometry().setFromPoints([
                nodes[i].position,
                nodes[j].position
              ]);
              const line = new THREE.Line(lineGeometry, lineMaterial);
              scene.add(line);
            }
          }
        }
        
        camera.position.z = 8;
        camera.position.y = 0;
        camera.lookAt(0, 0, 0);
        
        function animate() {
          requestAnimationFrame(animate);
          
          planet.rotation.y += 0.0005;
          planet.rotation.x += 0.0002;
          
          nodes.forEach(node => {
            node.rotation.y += 0.0005;
            node.rotation.x += 0.0002;
          });
          
          scene.children.forEach(child => {
            if (child.type === 'Line') {
              child.rotation.y += 0.0005;
              child.rotation.x += 0.0002;
            }
          });
          
          renderer.render(scene, camera);
        }
        
        animate();
        
        window.addEventListener('resize', function() {
          camera.aspect = window.innerWidth / window.innerHeight;
          camera.updateProjectionMatrix();
          renderer.setSize(window.innerWidth, window.innerHeight);
        });
      }
      
      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', initThreeJS);
      } else {
        initThreeJS();
      }
      
        document.addEventListener('DOMContentLoaded', function() {
    const field = document.getElementById('signup_password');
    const toggle = document.getElementById('toggle_password');

    toggle.addEventListener('click', function() {
      if (field.type === 'password') {
        field.type = 'text';
        toggle.textContent = '🙈';  // Hide icon
      } else {
        field.type = 'password';
        toggle.textContent = '👁️';  // Show icon
      }
    });
  });
    "))
  )
}


# SIGNUP UI ----

signup_ui <- function() {
  div(
    style = 
      "height: 100vh; 
    display: flex; 
    flex-direction: column; 
    justify-content: center; 
    align-items: center; 
    background-image: url('https://raw.github.com/ConnorFewster/EI/main/EI-Performance%20-%20Gradient%20Backgrounds-Green-1.png');
    background-size: cover;
    background-position: center;
    position: relative;
    overflow: hidden;",
    
    # Three.js canvas container (background)
    div(
      id = "threejs-container-signup",
      style = "
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      z-index: 1;"
    ),
    
    # Logo
    div(
      style = "
      text-align: center;
      margin-bottom: 20px;
      position: relative;
      z-index: 2;",
      
      img(src = "https://raw.github.com/ConnorFewster/EI/main/EI%20Performance%20-%20Logo%20-%20Icon%20-%20Green.png",
          style = "
          height: 140px;
          width: auto;
          margin-bottom: 20px;")
    ),
    
    # Signup form
    div(
      style = "
      background: rgba(255, 255, 255, 0.1); 
      padding: 30px 20px;
      border-radius: 10px; 
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); 
      width: 450px;
      text-align: center;
      position: relative;
      z-index: 2;
      backdrop-filter: blur(10px);",
      
      # Title
      div(
        style = "
        color: white;
        font-size: 24px;
        font-weight: bold;
        margin-bottom: 25px;
        font-family: 'Raleway', sans-serif;",
        "Create Account"
      ),
      
      # Forename
      div(
        style = "margin-bottom: 20px;",
        textInput("signup_forename", NULL,
                  placeholder = "Forename",
                  width = "100%")
      ),
      
      # Surname
      div(
        style = "margin-bottom: 20px;",
        textInput("signup_surname", NULL,
                  placeholder = "Surname",
                  width = "100%")
      ),
      
      # Email
      div(
        style = "margin-bottom: 20px;",
        textInput("signup_email", NULL,
                  placeholder = "Email Address",
                  width = "100%")
      ),
      
      # Username
      div(
        style = "margin-bottom: 20px;",
        textInput("signup_username", NULL,
                  placeholder = "Create a Username",
                  width = "100%")
      ),
      
      # Password input with toggle
      div(
        style = "margin-bottom: 20px; position: relative; width: 100%;",
        
        tags$input(
          id = "signup_password",
          type = "password",
          class = "form-control",
          placeholder = "Create a Password",
          style = "
      width: 100%;
      padding-right: 50px;     /* space for eye icon */
      height: 45px;
      box-sizing: border-box;
    "
        ),
        
        # Eye icon inside the input box
        tags$span(
          id = "signup_toggle_password",
          style = "
      position: absolute;
      right: 15px;
      top: 50%;
      transform: translateY(-50%);
      cursor: pointer;
      color: rgba(255, 255, 255, 0.9);
      font-size: 20px;
      user-select: none;
      z-index: 20;
    ",
          HTML("👁️")
        )
      ),
      
      # Role dropdown
      div(
        style = "margin-bottom: 25px;",
        selectInput("signup_role", NULL,
                    choices = c("Select Role" = "", "Analyst", "Coach", "Player", 
                                "Referee", "Student", "Other"),
                    width = "100%")
      ),
      
      # Signup button
      actionButton("signup_submit", "Sign Up", 
                   class = "btn-primary btn-lg",
                   style = "
                   width: 100%; 
                   padding: 12px;
                   background-color: #218838;
                   color: white;
                   border: none;
                   cursor: pointer;
                   margin-bottom: 15px;"),
      
      # Success/Error message
      div(
        id = "signup_message",
        style = "margin-top: 15px; text-align: center; font-weight: bold;",
        uiOutput("signup_status")
      )
    ),
    
    # Three.js script
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    
    # Custom CSS
    tags$style(HTML("
      .form-control {
        background: rgba(255, 255, 255, 0.1) !important;
        border: 2px solid rgba(255, 255, 255, 0.3) !important;
        color: white !important;
        font-family: 'Raleway', sans-serif !important;
        border-radius: 8px !important;
        padding: 12px !important;
      }
      
      .form-control:focus {
        background: rgba(255, 255, 255, 0.15) !important;
        border-color: #00ff88 !important;
        box-shadow: 0 0 10px rgba(0, 255, 136, 0.3) !important;
        color: white !important;
      }
      
      .form-control::placeholder {
        color: rgba(255, 255, 255, 0.6) !important;
      }
      
      select.form-control option {
        background-color: #1a1a1a !important;
        color: white !important;
      }
      
      #signup_submit:hover {
        background-color: #1e7e34 !important;
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
      }
      
      #back_to_login:hover {
        color: #00ffaa !important;
        text-shadow: 0 0 10px rgba(0, 255, 136, 0.5);
      }
    ")),
    
    # JavaScript for back to login
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        const backLink = document.getElementById('back_to_login');
        
        if (backLink) {
          backLink.addEventListener('click', function(event) {
            event.preventDefault();
            Shiny.setInputValue('back_to_login', Math.random());
          });
        }
      });
    ")),
    
    # Three.js animation (same as login)
    tags$script(HTML("
      function initThreeJSSignup() {
        if (typeof THREE === 'undefined') {
          setTimeout(initThreeJSSignup, 100);
          return;
        }
        
        const container = document.getElementById('threejs-container-signup');
        if (!container) {
          return;
        }
        
        const scene = new THREE.Scene();
        const camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
        const renderer = new THREE.WebGLRenderer({ alpha: true, antialias: true });
        
        renderer.setSize(window.innerWidth, window.innerHeight);
        renderer.setClearColor(0x000000, 0);
        container.appendChild(renderer.domElement);
        
        const planetRadius = 6.5;
        const widthSegments = 16;
        const heightSegments = 12;
        const geometry = new THREE.SphereGeometry(planetRadius, widthSegments, heightSegments);
        const material = new THREE.MeshBasicMaterial({
          color: 0x808080,
          wireframe: true,
          transparent: true,
          opacity: 0.1
        });
        const planet = new THREE.Mesh(geometry, material);
        scene.add(planet);
        
        const nodeGeometry = new THREE.SphereGeometry(0.08, 8, 8);
        const nodeMaterial = new THREE.MeshBasicMaterial({ 
          color: 0x808080,
          transparent: true,
          opacity: 0.1
        });
        const nodes = [];
        
        for (let i = 0; i <= heightSegments; i++) {
          const theta = (i / heightSegments) * Math.PI;
          for (let j = 0; j <= widthSegments; j++) {
            const phi = (j / widthSegments) * Math.PI * 2;
            
            const node = new THREE.Mesh(nodeGeometry, nodeMaterial);
            
            node.position.x = planetRadius * Math.sin(theta) * Math.cos(phi);
            node.position.y = planetRadius * Math.cos(theta);
            node.position.z = planetRadius * Math.sin(theta) * Math.sin(phi);
            
            nodes.push(node);
            scene.add(node);
          }
        }
        
        const lineMaterial = new THREE.LineBasicMaterial({ 
          color: 0x808080,
          transparent: true, 
          opacity: 0.1
        });
        
        for (let i = 0; i < nodes.length; i++) {
          for (let j = i + 1; j < nodes.length; j++) {
            const distance = nodes[i].position.distanceTo(nodes[j].position);
            if (distance < 4.0 && Math.random() > 0.85) {
              const lineGeometry = new THREE.BufferGeometry().setFromPoints([
                nodes[i].position,
                nodes[j].position
              ]);
              const line = new THREE.Line(lineGeometry, lineMaterial);
              scene.add(line);
            }
          }
        }
        
        camera.position.z = 8;
        camera.position.y = 0;
        camera.lookAt(0, 0, 0);
        
        function animate() {
          requestAnimationFrame(animate);
          
          planet.rotation.y += 0.0005;
          planet.rotation.x += 0.0002;
          
          nodes.forEach(node => {
            node.rotation.y += 0.0005;
            node.rotation.x += 0.0002;
          });
          
          scene.children.forEach(child => {
            if (child.type === 'Line') {
              child.rotation.y += 0.0005;
              child.rotation.x += 0.0002;
            }
          });
          
          renderer.render(scene, camera);
        }
        
        animate();
        
        window.addEventListener('resize', function() {
          camera.aspect = window.innerWidth / window.innerHeight;
          camera.updateProjectionMatrix();
          renderer.setSize(window.innerWidth, window.innerHeight);
        });
      }
      
      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', initThreeJSSignup);
      } else {
        initThreeJSSignup();
      }
      
       document.addEventListener('click', function(e) {
    const field = document.getElementById('signup_password');
    const toggle = document.getElementById('signup_toggle_password');

    if (!field || !toggle) return;

    if (e.target === toggle) {
      if (field.type === 'password') {
        field.type = 'text';
        toggle.textContent = '🙈';
      } else {
        field.type = 'password';
        toggle.textContent = '👁️';
      }
    }
  });
    "))
  )
}

# MATCHES DASHBOARD UI ----

matches_ui <- function() {
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      img(src = "https://raw.githubusercontent.com/ConnorFewster/EI/main/EI%20Performance%20-%20Logo%20-%20Icon%20-%20Green.png", 
          class = "logo-img"),
      h1("Uploads"),
      actionButton("upload", "Upload", icon = icon("upload")),
      div(class = "table-container",
          DTOutput("matches_table")
      ),
      tags$head(
        tags$style(HTML("
          body, .content-wrapper {
            background: url('https://raw.githubusercontent.com/ConnorFewster/EI/main/EI-Performance%20-%20Gradient%20Backgrounds-Green-7.png') no-repeat center center fixed;
            background-size: cover;
          }
          
          .logo-img {
            height: 50px;
            width: auto;
            margin: 20px;
          }
          
          h1 {
            text-align: center;
            font: Arial, sans-serif;
            color: white;
            font-weight: bold;
            margin-top: -55px !important;
            padding-top: 0 !important;
          }
          
          h2 {
            color: white;
            font-weight: bold;
            text-align: center;
            margin-top: 40px;
            margin-bottom: 20px;
          }
          
          #upload {
            border-radius: 10px;
            border-color: green;
            font-family: Arial, sans-serif;
            background-color: transparent !important;
            color: white;
            margin-top: -80px;
            margin-left: 1650px;
          }
          
          #upload:hover {
            background-color: black;
            transform: scale(1.1);
            transition: transform 0.3s, background-color 0.3s;
          }
          
          .table-container {
            margin: 40px auto;
            max-width: 1800px;
            padding: 20px;
            border: 1px solid rgba(255, 255, 255, 0.2);
            border-radius: 20px;
            background: rgba(100, 100, 100, 0.02);
            backdrop-filter: blur(15px);
            -webkit-backdrop-filter: blur(15px);
          }
          
          .dataTables_wrapper {
            color: white !important;
            border-color: white;
          }
          
          .dataTables_wrapper .dataTables_length,
          .dataTables_wrapper .dataTables_filter,
          .dataTables_wrapper .dataTables_info,
          .dataTables_wrapper .dataTables_paginate {
            color: white !important;
          }
          
          /* --- DataTable Styling --- */
          table.dataTable thead th {
          background-color: transparent !important;
          color: white !important;
          text-align: center !important;
          vertical-align: middle !important;
          }

          /* Cell text styling */
          table.dataTable tbody td {
          background-color: transparent !important;
          color: white !important;
          text-align: center;
          padding: 12px 15px !important; 
          }

         /* Row card-like appearance */
          table.dataTable tbody tr {
          background-color: rgba(255, 255, 255, 0.05) !important;
          border: 1px solid rgba(255, 255, 255, 0.2);
          border-radius: 15px;
          overflow: hidden;
          transition: all 0.3s ease;
          }

         /* Add visible space between rows */
          table.dataTable tbody {
          border-collapse: separate !important;
          border-spacing: 0 15px !important; 
          }

         /* Hover effect for row lift + color */
          table.dataTable tbody tr:hover {
          background-color: rgba(0, 128, 0, 0.25) !important;
          transform: scale(1.02);
          box-shadow: 0 4px 10px rgba(0, 0, 0, 0.3);
          }

         /* Make sure header stands out a bit */
          table.dataTable thead th {
          background-color: rgba(150, 150, 150, 0.2) !important;
          backdrop-filter: blur(10px);
          }

          
          table.dataTable tbody tr td {
          padding 12px 15px;
          }
          
          table.dataTable tbody {
          border-spacing: o 15px !important;
          border-collapse: separate !important;
          }
          
         table.dataTable tbody tr:hover {
         background-color: rgba(0, 128, 0, 0.25) !important;
         box-shadow: inset 0 0 10px rgba(0, 255, 136, 0.4);
         transition: background-color 0.3s, box-shadow 0.3s;
          }

          .dataTables_wrapper .dataTables_filter input {
            background-color: rgba(255, 255, 255, 0.1) !important;
            border: 1px solid rgba(255, 255, 255, 0.2) !important;
            color: white !important;
            border-radius: 5px;
            padding: 5px 10px;
          }
          
          .modal-backdrop {
            background-color: rgba(0, 0, 0, 0.5) !important;
            backdrop-filter: blur(10px);
            -webkit-backdrop-filter: blur(10px);
          }
          
          .modal-dialog {
            display: flex;
            align-items: center;
            justify-content: center;
            min-height: calc(100vh - 60px);
            max-width: 800px;
          }
          
          .modal-content {
            background-color: rgba(26, 26, 26, 0.95) !important;
            color: white;
            border-radius: 15px;
            border: 1px solid rgba(255, 255, 255, 0.1);
            backdrop-filter: blur(20px);
            -webkit-backdrop-filter: blur(20px);
            box-shadow: 0 8px 32px rgba(0, 0, 0, 0.4);
          }
          
          .modal-header {
            border-bottom: 1px solid rgba(255, 255, 255, 0.1);
            background-color: transparent;
          }
          
          .modal-title {
            color: white;
            font-weight: bold;
          }
          
          .modal-footer {
            border-top: 1px solid rgba(255, 255, 255, 0.1);
            background-color: transparent;
          }
          
          .modal-body {
            background-color: transparent;
          }
          
          .video-container {
            position: relative;
            width: 100%;
            padding-bottom: 56.25%;
            margin-bottom: 20px;
            border-radius: 10px;
            overflow: hidden;
            background-color: rgba(0, 0, 0, 0.3);
          }
          
          .video-container iframe {
            position: absolute;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            border: none;
            border-radius: 20px;
          }
          
          .form-control {
            background-color: rgba(255, 255, 255, 0.1) !important;
            border: 1px solid rgba(255, 255, 255, 0.2) !important;
            color: white !important;
            border-radius: 8px;
          }
          
          .form-control:focus {
            background-color: rgba(255, 255, 255, 0.15) !important;
            border-color: rgba(255, 255, 255, 0.4) !important;
            box-shadow: 0 0 0 0.2rem rgba(255, 255, 255, 0.1) !important;
          }
          
          .form-control::placeholder {
            color: rgba(255, 255, 255, 0.5) !important;
          }
          
          label {
            color: white;
            font-weight: 500;
          }
          
          .close {
            color: transparent !important;
            opacity: 0.8;
            text-shadow: none;
          }
          
          .close:hover {
            opacity: 1;
          }
          
          #preview_video {
            margin-top: 10px;
            margin-bottom: 10px;
            background-color: transparent;
            color: white;
            border-color: green;
            border-radius: 10px;
          }
          
          #preview_video:hover {
            background-color: transparent;
            transform: scale(1.05);
            transition: transform 0.3s, background-color 0.3s;
          }
          
          .btn-cancel {
            background-color: transparent;
            border-radius: 10px;
            border-color: green;
            color: white;
          }
          
          .btn-cancel:hover{
            background-color: transparent !important;
            transform: scale(1.1);
            transition: transform 0.3s, background-color 0.3s;
            border-color: green;
            color: white;
          }
          
          .btn-primary {
            background-color: transparent;
            border-radius: 10px;
            border-color: green;
            color: white
          }
          
          .btn-primary:hover{
            background-color: transparent !important;
            transform: scale(1.1);
            transition: transform 0.3s, background-color 0.3s;
            border-color: green;
          }
        "))
      )
    )
  )
}


# ANALYSIS UI ----

pitch_length <- 100
pitch_width <- 70

extract_youtube_id <- function(url) {
  if (grepl("youtu.be/", url)) {
    id <- gsub(".*youtu.be/([^?&]*)", "\\1", url)
  } else if (grepl("youtube.com/watch", url)) {
    id <- gsub(".*[?&]v=([^&]*)", "\\1", url)
  } else if (grepl("youtube.com/embed/", url)) {
    id <- gsub(".*youtube.com/embed/([^?&]*)", "\\1", url)
  } else if (grepl("youtube.com/live/", url)) {
    id <- gsub(".*youtube.com/live/([^?&]*)", "\\1", url)
  } else {
    return(NULL)
  }
  id <- gsub("[?&].*", "", id)
  id
}

is_valid_youtube_url <- function(url) {
  if (is.null(url) || url == "") return(FALSE)
  if (!grepl("youtube\\.com|youtu\\.be", url, ignore.case = TRUE)) return(FALSE)
  has_watch <- grepl("youtube\\.com/watch\\?v=", url, ignore.case = TRUE)
  has_short <- grepl("youtu\\.be/", url, ignore.case = TRUE)
  has_embed <- grepl("youtube\\.com/embed/", url, ignore.case = TRUE)
  has_live <- grepl("youtube\\.com/live/", url, ignore.case = TRUE)
  has_watch || has_short || has_embed || has_live
}

draw_pitch <- function() {
  ggplot() +
    # Background ----
  geom_rect(aes(xmin = 0, xmax = pitch_length, ymin = 0, ymax = 70), fill = "forestgreen") +
    
    # Theming ----
  theme_classic() +
    theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    ) +
    # Pitch Outline ----
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = pitch_width), linewidth = 1, colour = "white") + # vertical 
    geom_segment(aes(x = 100, y = 0, xend = 100, yend = pitch_width), linewidth = 1, colour = "white") + # vertical
    geom_segment(aes(x = 0, y = 0, xend = pitch_length, yend = 0), linewidth = 1, colour = "white") + # horizontal
    geom_segment(aes(x = 0, y = 70, xend = pitch_length, yend = pitch_width), linewidth = 1, colour = "white") + # horizontal
    # 22 & Halfway Line ----
  geom_segment(aes(x = 22, y = 0, xend = 22, yend = 70), linewidth = 1, colour = "white") + # 22
    geom_segment(aes(x = 50, y = 0, xend = 50, yend = 70), linewidth = 1, colour = "white") + # halfway
    geom_segment(aes(x = 78, y = 0, xend = 78, yend = 70), linewidth = 1, colour = "white") + # 22
    # 5 Metre Lines ----
  geom_segment(aes(x = 5, y = 2.5, xend = 5, yend = 7.5), linewidth = 1, colour = "white") + 
    geom_segment(aes(x = 5, y = 12.5, xend = 5, yend = 17.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 5, y = 22.5, xend = 5, yend = 27.5), linewidth = 1, colour = "white") + 
    geom_segment(aes(x = 5, y = 32.5, xend = 5, yend = 37.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 5, y = 42.5, xend = 5, yend = 47.5), linewidth = 1, colour = "white") + 
    geom_segment(aes(x = 5, y = 52.5, xend = 5, yend = 57.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 5, y = 62.5, xend = 5, yend = 67.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 95, y = 2.5, xend = 95, yend = 7.5), linewidth = 1, colour = "white") + 
    geom_segment(aes(x = 95, y = 12.5, xend = 95, yend = 17.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 95, y = 22.5, xend = 95, yend = 27.5), linewidth = 1, colour = "white") + 
    geom_segment(aes(x = 95, y = 32.5, xend = 95, yend = 37.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 95, y = 42.5, xend = 95, yend = 47.5), linewidth = 1, colour = "white") + 
    geom_segment(aes(x = 95, y = 52.5, xend = 95, yend = 57.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 95, y = 62.5, xend = 95, yend = 67.5), linewidth = 1, colour = "white") +
    # 10 Metre Lines ----
  geom_segment(aes(x = 40, y = 2.5, xend = 40, yend = 7.5), linewidth = 1, colour = "white") + 
    geom_segment(aes(x = 40, y = 12.5, xend = 40, yend = 17.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 40, y = 22.5, xend = 40, yend = 27.5), linewidth = 1, colour = "white") + 
    geom_segment(aes(x = 40, y = 32.5, xend = 40, yend = 37.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 40, y = 42.5, xend = 40, yend = 47.5), linewidth = 1, colour = "white") + 
    geom_segment(aes(x = 40, y = 52.5, xend = 40, yend = 57.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 40, y = 62.5, xend = 40, yend = 67.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 60, y = 2.5, xend = 60, yend = 7.5), linewidth = 1, colour = "white") + 
    geom_segment(aes(x = 60, y = 12.5, xend = 60, yend = 17.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 60, y = 22.5, xend = 60, yend = 27.5), linewidth = 1, colour = "white") + 
    geom_segment(aes(x = 60, y = 32.5, xend = 60, yend = 37.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 60, y = 42.5, xend = 60, yend = 47.5), linewidth = 1, colour = "white") + 
    geom_segment(aes(x = 60, y = 52.5, xend = 60, yend = 57.5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 60, y = 62.5, xend = 60, yend = 67.5), linewidth = 1, colour = "white") +
    # 5 Metre Channels ----
  geom_segment(aes(x = 5, y = 5, xend = 10, yend = 5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 19.5, y = 5, xend = 24.5, yend = 5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 37.5, y = 5, xend = 42.5, yend = 5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 47.5, y = 5, xend = 52.5, yend = 5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 57.5, y = 5, xend = 62.5, yend = 5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 75.5, y = 5, xend = 80.5, yend = 5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 90, y = 5, xend = 95, yend = 5), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 5, y = 65, xend = 10, yend = 65), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 19.5, y = 65, xend = 24.5, yend = 65), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 37.5, y = 65, xend = 42.5, yend = 65), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 47.5, y = 65, xend = 52.5, yend = 65), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 57.5, y = 65, xend = 62.5, yend = 65), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 75.5, y = 65, xend = 80.5, yend = 65), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 90, y = 65, xend = 95, yend = 65), linewidth = 1, colour = "white") +
    # 15 Metre Channels ----
  geom_segment(aes(x = 5, y = 15, xend = 10, yend = 15), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 19.5, y = 15, xend = 24.5, yend = 15), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 37.5, y = 15, xend = 42.5, yend = 15), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 47.5, y = 15, xend = 52.5, yend = 15), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 57.5, y = 15, xend = 62.5, yend = 15), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 75.5, y = 15, xend = 80.5, yend = 15), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 90, y = 15, xend = 95, yend = 15), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 5, y = 55, xend = 10, yend = 55), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 19.5, y = 55, xend = 24.5, yend = 55), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 37.5, y = 55, xend = 42.5, yend = 55), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 47.5, y = 55, xend = 52.5, yend = 55), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 57.5, y = 55, xend = 62.5, yend = 55), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 75.5, y = 55, xend = 80.5, yend = 55), linewidth = 1, colour = "white") +
    geom_segment(aes(x = 90, y = 55, xend = 95, yend = 55), linewidth = 1, colour = "white") +
    # Goal Posts ----
  geom_point(aes(x = 0, y = 32.5), colour = "white", size = 2) +
    geom_point(aes(x = 0, y = 37.5), colour = "white", size = 2) +
    geom_point(aes(x = 100, y = 32.5), colour = "white", size = 2) +
    geom_point(aes(x = 100, y = 37.5), colour = "white", size = 2) +
    theme(
      plot.background = element_rect(fill = "black",
                                     colour = "black"),
      panel.background = element_rect(fill = "black",
                                      colour = "black")
    )
}

analysis_ui <- function() {
  dashboardPage(skin = "black",
                dashboardHeader(disable = TRUE),
                dashboardSidebar(disable = TRUE),
                dashboardBody(
                  tags$head(
                    tags$style(HTML("
      body, .content-wrapper {
        background: url('https://raw.github.com/ConnorFewster/EI/main/EI-Performance%20-%20Gradient%20Backgrounds-Green-7.png') no-repeat center center fixed;
        background-size: cover;
      }
      
      .box {
        background-color: rgba(255, 255, 255, 0.01) !important;
        box-shadow: 0 4px 6px rgba(40, 167, 69, 0.4) !important;
        border: none !important;
        color: white !important;
      }
      
      .box .box-title {
        color: white !important;
      }
      
      .value-box {
        background-color: rgba(255, 255, 255, 0.1) !important;
        color: white !important;
      }
      
      table.dataTable { color: white; }
      .timer-style pre {
        color: white !important;
        font-size: 55.5px !important;
        font-weight: bold !important;
        text-align: center !important;
        background: transparent !important;
        border: none !important;
      }
      
      .video-container iframe {
      border: none;
      border-radius: 15px;
      }
    "))
                  ),
                  useShinyjs(),
                  div(
                    style = "padding: 10px 15px;",
                    actionButton("back_to_matches", "← Back to Matches",
                                 style = "background-color: transparent; border: 1px solid #28a745; color: white; border-radius: 8px;")
                  ),
                  # JS for YouTube & hotkeys
                  tags$script(HTML("
var timeoutId = null;
var player;
var playerReady = false;
var tag = document.createElement('script');
tag.src = 'https://www.youtube.com/iframe_api';
var firstScriptTag = document.getElementsByTagName('script')[0];
firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);
window.onYouTubeIframeAPIReady = function() {
  window.createPlayer = function(videoId) {
    if (player) { player.destroy(); }
    player = new YT.Player('youtube-player', {
      height: '700',
      width: '100%',
      videoId: videoId,
      playerVars: {
        'playsinline': 1,
        'controls': 1,
        'rel': 0
      },
      events: {
        'onReady': function(event) {
          playerReady = true;
        },
        'onStateChange': function(event) {
          if (event.data == YT.PlayerState.PLAYING) {
            Shiny.setInputValue('video_playing', true, {priority: 'event'});
          } else if (event.data == YT.PlayerState.PAUSED) {
            Shiny.setInputValue('video_playing', false, {priority: 'event'});
          }
        }
      }
    });
  };
};
$(document).on('keydown', function(e) {
  if (e.key === 'c') { Shiny.setInputValue('hotkey', 'Carry', {priority: 'event'}); }
  else if (e.key === 'r') { Shiny.setInputValue('hotkey', 'Ruck', {priority: 'event'}); }
  else if (e.key === 't') { Shiny.setInputValue('hotkey', 'Tackle', {priority: 'event'}); }
  else if (e.key === 'k') { Shiny.setInputValue('hotkey', 'Kick', {priority: 'event'}); }
  else if (e.key === 'p') { Shiny.setInputValue('hotkey', 'Pass', {priority: 'event'}); }
  else if (e.key === 'l') { Shiny.setInputValue('hotkey', 'Lineout', {priority: 'event'}); }
  else if (e.key === 's') { Shiny.setInputValue('hotkey', 'Scrum', {priority: 'event'}); }
  else if (e.key === 'y') { Shiny.setInputValue('hotkey', 'Try', {priority: 'event'}); }
  else if (e.key === 'a') { Shiny.setInputValue('hotkey', 'Try Assist', {priority: 'event'}); }
  else if (e.key === 'd') { Shiny.setInputValue('hotkey', 'Defender Beaten', {priority: 'event'}); }
  else if (e.key === 'o') { Shiny.setInputValue('hotkey', 'Offload', {priority: 'event'}); }
  else if (e.key === 'i') { Shiny.setInputValue('hotkey', 'Interception', {priority: 'event'}); }
  else if (e.key === 'e') { Shiny.setInputValue('hotkey', 'Error', {priority: 'event'}); }
  else if (e.key === 'g') { Shiny.setInputValue('hotkey', 'Possession', {priority: 'event'}); }
  else if (e.key === 'h') { Shiny.setInputValue('hotkey', 'Kick Received', {priority: 'event'}); }
  else if (e.key === 'u') { Shiny.setInputValue('hotkey', 'Substitution', {priority: 'event'}); }
  else if (e.key === 'w') { Shiny.setInputValue('hotkey', 'Pen Won', {priority: 'event'}); }
  else if (e.key === 'x') { Shiny.setInputValue('hotkey', 'Pen Conceded', {priority: 'event'}); }
  else if (e.key === 'z') { Shiny.setInputValue('hotkey', 'Card', {priority: 'event'}); }
  else if (e.key === 'v') { Shiny.setInputValue('hotkey', 'Turnover Won', {priority: 'event'}); }
  else if (e.key >= '1' && e.key <= '2') {
    var firstDigit = parseInt(e.key);
    if (timeoutId !== null) {
      clearTimeout(timeoutId);
    }
    $(document).off('keydown.numberCombo').on('keydown.numberCombo', function(e2) {
      if (e2.key >= '0' && e2.key <= '9') {
        var secondDigit = parseInt(e2.key);
        var playerNumber = firstDigit * 10 + secondDigit;
        if (playerNumber >= 10 && playerNumber <= 23) {
          Shiny.setInputValue('hotkey_id_number', playerNumber, {priority: 'event'});
        }
        clearTimeout(timeoutId);
        timeoutId = null;
        $(document).off('keydown.numberCombo');
        e2.preventDefault();
      } else {
        clearTimeout(timeoutId);
        timeoutId = null;
        $(document).off('keydown.numberCombo');
      }
    });
    timeoutId = setTimeout(function() {
      Shiny.setInputValue('hotkey_id_number', firstDigit, {priority: 'event'});
      timeoutId = null;
      $(document).off('keydown.numberCombo');
    }, 1000);
    e.preventDefault();
  } else if (e.key >= '3' && e.key <= '9') {
    var playerNumber = parseInt(e.key);
    Shiny.setInputValue('hotkey_id_number', playerNumber, {priority: 'event'});
  }
});
$(document).on('keydown', function(e) {
  if (!playerReady || !player) return;
  if (e.key === ' ' || e.key === 'ArrowLeft' || e.key === 'ArrowRight') {
    e.preventDefault();
  }
  if (e.key === ' ') {
    const state = player.getPlayerState();
    if (state === YT.PlayerState.PLAYING) {
      player.pauseVideo();
    } else if (state === YT.PlayerState.PAUSED || state === YT.PlayerState.UNSTARTED) {
      player.playVideo();
    }
  } else if (e.key === 'ArrowLeft') {
    const current = player.getCurrentTime();
    player.seekTo(Math.max(current - 5, 0), true);
  } else if (e.key === 'ArrowRight') {
    const current = player.getCurrentTime();
    const duration = player.getDuration();
    player.seekTo(Math.min(current + 5, duration), true);
  }
});
setInterval(function() {
  if (playerReady && player && typeof player.getCurrentTime === 'function') {
    var currentTime = player.getCurrentTime();
    Shiny.setInputValue('video_timer_seconds', Math.floor(currentTime), {priority: 'event'});
  }
}, 1000);
window.syncVideoToTimer = function(seconds) {
  if (playerReady && player) {
    player.seekTo(seconds, true);
  }
};
$(document).ready(function() {
  window.focus();
  $(document).on('click mousemove', function() {
    window.focus();
  });
  setInterval(function() {
    window.focus();
  }, 3000);
  $('iframe').on('load', function() {
    setTimeout(function() {
      window.focus();
    }, 500);
  });
});
")),
                  fluidRow(
                    box(
                      width = 6,  
                      status = "success",
                      uiOutput("video_player")
                    ),
                    box(
                      width = 4,
                      status = "success",
                      plotOutput("pitch", click = "pitch_click", width = "100%", height = "700px")
                    ),
                    box(
                      width = 2,
                      status = "success",
                      selectInput("id_number", "ID Number:", 
                                  choices = c("Unknown", "Team", "Opposition", as.character(1:60)), selected = NULL),
                      selectInput("venue", "Venue:", choices = c("Home", "Away")),
                      textInput("team", "Team:", value = ""),
                      textInput("opponent", "Opponent:", value = ""),
                      selectInput("half", "Half:", choices = c("First Half", "Second Half")),
                      selectInput("direction", "Playing Direction:", choices = c("Left to Right", "Right to Left")),
                      selectInput("event_type", "Event Type:", 
                                  choices = c("", "Carry", "Ruck", "Tackle", "Kick", "Possession", "Pen Conceded", "Pen Won",
                                              "Pass", "Kick Received", "Try", "Try Assist", "Lineout", "Scrum", "Card",
                                              "Substitution", "Defender Beaten", "Offload", "Interception", "Error", "Turnover Won")),
                      verbatimTextOutput("selected_event"),
                      conditionalPanel(
                        condition = "input.event_type == 'Carry'",
                        radioButtons("gls", "GLS:", choices = c(1, 0), selected = NULL),
                        radioButtons("linebreak", "Linebreak:", choices = c(1, 0), selected = NULL),
                        radioButtons("prior_gls", "Prior GLS?", choices = c(1, 0), selected = NULL),
                        selectInput("type", "Type:", choices = c("One Out", "Pick & Go", "Support Carry",
                                                                 "Kick Return", "Tap Pen", "Other"), selected = NULL),
                        selectInput("pass_receipt", "Pass Receipt:", choices = c("On Target", "Off Target", "NA"), selected = "NA"),
                        selectInput("source", "Source:", choices = c("Scrum", "Lineout", "Multi-Phase", "Penalty"), selected = "NA"),
                        textInput("defenders_infront", "Defenders Infront:", value = "")
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Tackle'",
                        radioButtons("tackle_type", "Tackle Type:", choices = c("Successful", "Missed", "Assist", "Offload Allowed"), selected = NULL),
                        radioButtons("outcome", "Outcome:", choices = c("Dominant", "Neutral", "Passive"), selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Kick'",
                        selectInput("kick_type", "Kick Type:", choices = c(
                          "Open Play", "Chip", "Touch Kick", "Box", "Conversion", 
                          "Penalty Goal", "Drop Goal", "Kick-Off", "Up & Under", "Grubber",
                          "Cross Field"
                        ), selected = NULL),
                        selectInput("kick_outcome", "Kick Outcome:", 
                                    choices = c("Retained", "50-22", "Error",
                                                "In Touch", "Opposition Collection Error",
                                                "Opposition Collection Full",
                                                "Opposition Collection Bounce", 
                                                "Successful", "Unsuccessful"))
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Possession'",
                        radioButtons("Possession_type", "Possession Type:", choices = c("Start", "End"), selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Pass'",
                        selectInput("pass_type", "Pass Type:", choices = c("Gainline Not Made", "Gainline Made", "Error", "Linebreak"))
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Try'",
                        radioButtons("try", "Try Outcome:", choices = c("Scored", "Held Up", "Penalty"), selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Try Assist'",
                        radioButtons("try_assist", "Try Assist Type:", choices = c("Pass", "Kick", "Other"), selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Lineout'",
                        selectInput("thrower", "Thrower ID:", choices = as.character(1:60),selected = NULL),
                        selectInput("receiver", "Receiver ID:", choices = as.character(1:60),selected = NULL),
                        selectInput("numbers", "Numbers:", choices = as.character(1:15),selected = NULL),
                        selectInput("lineout_destination", "Lineout Destination:", choices = c("Front", "Middle", 
                                                                                               "Back", "15m +", "No Jump Front"), selected = NULL),
                        selectInput("lineout_outcome", "Lineout Outcome:", choices = c("Successful", "Lost", "Pen Conceded", 
                                                                                       "Free Kick", "Opposition"), selected = NULL),
                        selectInput("lineout_secondary_outcome", "Secondary Outcome:", choices = c("Down and Drive", "Off Top", 
                                                                                                   "Down and Peel", "Loose Ball", 
                                                                                                   "Catch and Run", "NA", "Lost"), selected = NULL),
                        selectInput("contested", "Opposition Contest:", choices = c("Direct", "Indirect", "None"))
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Scrum'",
                        selectInput("scrum_outcome", "Scrum Outcome:", choices = c("Won", "Lost", "Pen Won",
                                                                                   "Pen Con", "Opposition", "Reset"), selected = NULL),
                        selectInput("scrum_dominance", "Scrum Dominance:", choices = c("Dominant", "Neutral", "Passive"), selected = NULL),
                        selectInput("scrum_secondary_outcome", "Secondary Outcome:", choices = c("Eight Pick", "Eight Pass",
                                                                                                 "Nine Pass", "Nine Pick",
                                                                                                 "Pen Won", "Pen Con", "Reset"), selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Pen Conceded'",
                        selectInput("pen_type", "Pen Type:", choices = c(
                          "High Tackle", "Offside", "Not Rolling", "Not Releasing", 
                          "Holding On", "Playing 9", "Taking Man in Air", 
                          "Playing Ball in Ruck", "Official Abuse", "Fighting", 
                          "Foul Play", "Scrum", "Lineout", "Maul Collapse", 
                          "Maul Bind Change", "Maul Other", "Off Feet", "Side Entry", "Obstruction",
                          "Deliberate Knock On"
                        ), selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Pen Won'",
                        selectInput("pen_won_type", "Pen Won Type:", choices = c("Scrum", "Lineout", "Jackel",
                                                                                 "High Tackle", "Off Feet", "Offside",
                                                                                 "Not Rolling", "Not Releasing", "Playing 9",
                                                                                 "Taking Man in Air", "Playing Ball in Ruck",
                                                                                 "Official Abuse", "Fighting", "Foul Play",
                                                                                 "Maul Collapse", "Maul Bind Change", "Maul Other",
                                                                                 "Side Entry", "Obstruction", "Deliberate Knock On",
                                                                                 "Jumping Across"), selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Kick Received'",
                        radioButtons("kick_source", "Kick Source:", choices = c("Own Team", "Opposition"), selected = NULL),
                        radioButtons("catch_pressure", "Pressure:", choices = c("Yes", "No"), selected = NULL),
                        radioButtons("kick_received", "Kick Receipt Outcome:", choices = c("Successful", "Unsuccessful", "Tap Back"), selected = NULL),
                        selectInput("kick_type_received", "Type:", choices = c("Cross Field", "Box", "Chip", 
                                                                               "High Ball", "Bouncing Ball", "Grubber", 
                                                                               "Punt", "Kick Off"))
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Card'",
                        radioButtons("card_colour", "Card Colour:", choices = c("Yellow", "Red"), selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Substitution'",
                        selectInput("player_on", "Player On:", choices = as.character(1:60),selected = NULL),
                        selectInput("player_off", "Player Off:", choices = as.character(1:60),selected = NULL),
                        radioButtons("sub_type", "Substitution Type:", choices = c("Tactical", "HIA", "Blood", "Injury"),selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Defender Beaten'",
                        radioButtons("defender_beaten_type", "Beaten By:", choices = c("Ran Over", 
                                                                                       "Fend",
                                                                                       "Step", 
                                                                                       "Ran Round", 
                                                                                       "Linebreak", 
                                                                                       "Chaser"), selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Offload'",
                        radioButtons("offload", "Offload Outcome :", choices = c("Successful", 
                                                                                 "Unsuccessful"), selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Interception'",
                        radioButtons("interception", "Interception Outcome :", choices = c("Successful", 
                                                                                           "Unsuccessful"), selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Error'",
                        radioButtons("error", "Error Descriptor:", choices = c("Handling",
                                                                               "Pass"), selected = NULL),
                        selectInput("error_outcome", "Error Outcome:", choices = c("Knock On",
                                                                                   "Backwards", 
                                                                                   "To Floor", 
                                                                                   "Interception",
                                                                                   "Forward"), selected = NULL)
                      ),
                      conditionalPanel(
                        condition = "input.event_type == 'Turnover Won'",
                        selectInput("turnover_won_outcome", "Turnover Won Outcome:", choices = c("Tackle",
                                                                                                 "Opponent in Touch", 
                                                                                                 "Lineout Steal",
                                                                                                 "Scrum", 
                                                                                                 "Loose Ball"))
                      ),
                      textInput("additional_info", "Additional Info:", value = ""),
                      h5("Click the pitch to log events"),
                      actionButton("delete_row", "Delete Row", class = "btn-danger"),
                      br(), br(),
                      downloadButton("download_data", "Download Data as CSV", class = "btn-success")
                    )
                  ),
                  fluidRow(
                    box(
                      width = 12,
                      div(
                        style = "float: right; margin-top: -5px",
                        radioButtons("table_view", NULL,
                                     choices = c("Data View" = "detailed",
                                                 "Summary" = "summary"),
                                     selected = "summary",
                                     inline = TRUE)
                      ),
                      status = "success",
                      conditionalPanel(
                        condition = "input.table_view == 'detailed'",
                        DTOutput("dataTable")
                      ),
                      conditionalPanel(
                        condition = "input.table_view == 'summary'",
                        DTOutput("summaryTable")
                      )
                    )
                  )
                )
  )
}


# ROOT UI ----

ui <- fluidPage(
  useShinyjs(),
  uiOutput("page")
)


# SERVER ----

server <- function(input, output, session) {
  
  # auth + page
  authenticated <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  matches_data <- reactiveVal(data.frame())
  app_state <- reactiveVal("login")
  
  # matches modal
  video_embed_url <- reactiveVal(NULL)
  
  # lag to prevent match row navigation when editing
  session$userData$editing_match <- FALSE
  
  # Navigation to Sign Up Page
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$page) && query$page == "signup") {
      app_state("signup")
    }
  })
  
  # selected match info to pass into analysis
  session$userData$selected_video_url <- NULL
  session$userData$selected_home <- NULL
  session$userData$selected_away <- NULL
  
  # analysis data (this will hold Supabase EIP_Data rows for that match)
  analysis_data <- reactiveVal(NULL)
  
  # render right page
  output$page <- renderUI({
    if (app_state() == "login") {
      login_ui()
    } else if (app_state() == "signup") {
      signup_ui()
    } else if (app_state() == "matches") {
      matches_ui()
    } else {
      analysis_ui()
    }
  })
  
  # Navigate to signup page
  observeEvent(input$show_signup, {
    app_state("signup")
  })
  
  # Navigate back to login page
  observeEvent(input$back_to_login, {
    app_state("login")
  })
  
  observeEvent(input$login, {
    username <- trimws(input$username %||% "")
    password <- trimws(input$password %||% "")
    
    # Quick empty check
    if (username == "" || password == "") {
      output$login_message <- renderUI({
        tags$p("Please enter both username and password.", style = "color: #dc3545;")
      })
      cat("LOGIN FAILED: Missing username or password\n\n")
      return()
    }
    
    cat("\n=== LOGIN ATTEMPT ===\n")
    cat("Username entered:", username, "\n")
    cat("Password entered:", password, "\n")
    
    valid_credentials <- NULL
    credential_source <- NULL
    
    # --- 1. Try Supabase ---
    supabase_user <- tryCatch({
      result <- supabase_select(
        "login_credentials",
        select_clause = "username,password",
        filter = list(username = paste0("eq.", username))
      )
      cat("Supabase query returned:\n")
      print(result)
      result
    }, error = function(e) {
      cat("Supabase error:", e$message, "\n")
      NULL
    })
    
    if (is.data.frame(supabase_user) && nrow(supabase_user) > 0) {
      valid_credentials <- supabase_user[1, , drop = FALSE]
      credential_source <- "supabase"
      cat("Credentials found in Supabase.\n")
    }
    
    # --- 2. Fallback to local credentials ---
    if (is.null(valid_credentials)) {
      if (username %in% user_credentials$username) {
        valid_credentials <- user_credentials[user_credentials$username == username, , drop = FALSE]
        credential_source <- "local"
        cat("Credentials found in local fallback list.\n")
      } else {
        cat("No matching username in Supabase or local.\n")
      }
    }
    
    # --- 3. If no credentials at all ---
    if (is.null(valid_credentials)) {
      output$login_message <- renderUI({
        tags$p("Invalid username. Please try again.", style = "color: #dc3545;")
      })
      cat("LOGIN FAILED: Username not found.\n\n")
      return()
    }
    
    # --- 4. Extract + normalise stored password ---
    stored_password <- trimws(as.character(valid_credentials$password[1] %||% ""))
    cat("Stored password:", stored_password, "\n")
    
    # --- 5. Password comparison ---
    passwords_match <- isTRUE(password == stored_password)
    cat("Passwords match:", passwords_match, "\n")
    
    if (!passwords_match) {
      output$login_message <- renderUI({
        tags$p("Invalid password. Please try again.", style = "color: #dc3545;")
      })
      cat("LOGIN FAILED: Password mismatch.\n\n")
      return()
    }
    
    # --- 6. SUCCESS ---
    resolved_username <- trimws(as.character(valid_credentials$username[1] %||% username))
    current_user(resolved_username)
    authenticated(TRUE)
    
    app_state("matches")
    updateQueryString("?page=matches", mode = "replace", session = session)
    load_matches_data(resolved_username)
    
    output$login_message <- renderUI({ NULL })
    
    msg <- if (credential_source == "supabase") 
      "Login successful!" 
    else 
      "Login successful (legacy account)!"
    
    showNotification(msg, type = "message")
    
    cat("LOGIN SUCCESS for user:", resolved_username, "\n")
    cat("=== END LOGIN ATTEMPT ===\n\n")
  })
  
  # Handle signup submission
  observeEvent(input$signup_submit, {
    # Validate inputs
    if (input$signup_forename == "" || input$signup_surname == "" || 
        input$signup_email == "" || input$signup_role == "" ||
        input$signup_username == "" || input$signup_password == "") {
      output$signup_status <- renderUI({
        div(style = "color: #dc3545;", "Please fill in all fields.")
      })
      return()
    }
    
    # Validate email format
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", input$signup_email)) {
      output$signup_status <- renderUI({
        div(style = "color: #dc3545;", "Please enter a valid email address.")
      })
      return()
    }
    
    # Prepare data for Supabase
    signup_data <- list(
      forename = input$signup_forename,
      surname = input$signup_surname,
      email = input$signup_email,
      username = input$signup_username,
      password = input$signup_password,
      role = input$signup_role,
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    
    # Insert into Supabase
    response <- supabase_insert("login_credentials", signup_data)
    
    # Check response
    if (status_code(response) %in% c(200, 201)) {
      output$signup_status <- renderUI({
        div(style = "color: #28a745;", "Account created successfully! Redirecting to login...")
      })
      
      # Redirect to login after 2 seconds
      later(function() {
        app_state("login")
      }, 2)
      
    } else {
      output$signup_status <- renderUI({
        div(style = "color: #dc3545;", 
            paste("Error creating account. Please try again.", 
                  "(Status:", status_code(response), ")"))
      })
    }
  })
  
  # load matches
  load_matches_data <- function(username_override = NULL) {
    active_username <- username_override
    if (is.null(active_username)) {
      active_username <- current_user()
    }
    
    if (is.null(active_username) || !nzchar(active_username)) {
      matches_data(data.frame(
        Date = character(), Home = character(), Away = character(),
        Score = character(), Referee = character(), Options = character(),
        HiddenURL = character(), HiddenID = character(),
        stringsAsFactors = FALSE
      ))
      return(invisible(NULL))
    }
    data <- supabase_select(
      "matches", 
      order_by = "match_date.desc",
      filter = list(username = paste0("eq.", active_username))
    )
    if (!is.null(data) && is.data.frame(data) && nrow(data) > 0) {
      formatted <- data.frame(
        Date = as.character(data$match_date),
        Home = data$home_team,
        Away = data$away_team,
        Score = ifelse(is.na(data$score) | data$score == "", "-", data$score),
        Referee = ifelse(is.na(data$referee) | data$referee == "", "-", data$referee),
        
        # ▼ Three dots instead of Watch link
        Options = sprintf(
          '<span class="edit-match" data-row="%s" style="cursor:pointer; font-size:22px; color:white;">&#8942;</span>',
          seq_len(nrow(data))
        ),
        
        # Hidden metadata
        HiddenURL = data$youtube_url,
        HiddenID = if ("id" %in% names(data)) data$id else NA,
        stringsAsFactors = FALSE
      )
      matches_data(formatted)
    } else {
      matches_data(data.frame(
        Date = character(), Home = character(), Away = character(),
        Score = character(), Referee = character(), Options = character(),
        HiddenURL = character(), HiddenID = character(),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  observe({
    if (authenticated() && app_state() == "matches") {
      load_matches_data()
    }
  })
  
  # matches table
  output$matches_table <- renderDT({
    req(app_state() == "matches")
    df <- matches_data()
    
    # Hide hidden columns
    hide_targets <- which(names(df) %in% c("HiddenURL", "HiddenID")) - 1
    
    datatable(
      df,
      escape = FALSE,
      options = list(
        pageLength = 10,
        dom = 'frtip',
        ordering = TRUE,
        order = list(list(0, 'desc')),
        columnDefs = list(list(visible = FALSE, targets = hide_targets))
      ),
      rownames = FALSE,
      selection = "single",
      class = 'cell-border stripe',
      
      # ✅ FIX: prevent row selection when clicking the 3 dots
      callback = JS("
        table.on('click', 'span.edit-match', function(e) {
          e.stopPropagation(); 
          var row = $(this).data('row');
          Shiny.setInputValue('edit_match_click', row, {priority:'event'});
        });
      ")
    )
  })
  
  
  # show upload modal
  observeEvent(input$upload, {
    req(authenticated())
    req(app_state() == "matches")
    video_embed_url(NULL)
    showModal(modalDialog(
      title = "Match Details",
      size = "l",
      textInput("youtube_url", "YouTube Video Link:", 
                placeholder = "Paste YouTube video URL (works with regular videos and live streams)"),
      actionButton("preview_video", "Preview Video", class = "btn-info btn-sm"),
      uiOutput("video_preview"),
      hr(style = "border-color: rgba(255, 255, 255, 0.1);"),
      textInput("home_team", "Home Team:", placeholder = "Enter home team name"),
      textInput("away_team", "Away Team:", placeholder = "Enter away team name"),
      textInput("referee", "Referee:", placeholder = "Enter referee name"),
      textInput("score", "Score:", placeholder = "e.g., 2-1"),
      dateInput("match_date", "Match Date:", value = Sys.Date(), format = "dd/mm/yyyy"),
      footer = tagList(
        actionButton("cancel_btn", "Cancel", class = "btn-cancel", `data-dismiss` = "modal"),
        actionButton("submit_match", "Submit", class = "btn-primary")
      ),
      easyClose = TRUE,
      fade = TRUE
    ))
  })
  
  # ▼▼ EDIT MATCH MODAL ▼▼
  observeEvent(input$edit_match_click, {
    req(authenticated())
    req(app_state() == "matches")
    req(matches_data())
    
    # ✅ DO NOT trigger analysis navigation
    session$userData$editing_match <- TRUE  
    
    row <- as.numeric(input$edit_match_click)
    selected <- matches_data()[row, ]
    
    video_embed_url(NULL)
    
    showModal(modalDialog(
      title = "Edit Match Details",
      size = "l",
      
      textInput(
        "youtube_url", "YouTube Video Link:",
        value = if (!is.null(selected$HiddenURL)) selected$HiddenURL else "",
        placeholder = "Paste YouTube video URL"
      ),
      
      actionButton("preview_video", "Preview Video", class = "btn-info btn-sm"),
      uiOutput("video_preview"),
      
      hr(style = "border-color: rgba(255, 255, 255, 0.1);"),
      
      textInput("home_team", "Home Team:", value = selected$Home),
      textInput("away_team", "Away Team:", value = selected$Away),
      textInput("referee", "Referee:", value = ifelse(selected$Referee == "-", "", selected$Referee)),
      textInput("score", "Score:", value = ifelse(selected$Score == "-", "", selected$Score)),
      dateInput("match_date", "Match Date:", value = as.Date(selected$Date)),
      
      footer = tagList(
        actionButton("cancel_btn", "Cancel", class = "btn-cancel", `data-dismiss` = "modal"),
        actionButton("save_match_edits", "Save", class = "btn-primary")
      ),
      
      easyClose = TRUE,
      fade = TRUE
    ))
    
    # store ID for PATCH
    session$userData$edit_row_index <- row
    session$userData$edit_row_id <- if (!is.null(selected$HiddenID)) selected$HiddenID else NA
  })
  
  
  # ▼▼ NEW: SAVE EDITS BACK TO SUPABASE ▼▼
  observeEvent(input$save_match_edits, {
    req(authenticated())
    req(app_state() == "matches")
    
    row_id <- session$userData$edit_row_id
    if (is.null(row_id) || is.na(row_id)) {
      showNotification("Missing match ID; cannot update.", type = "error")
      return()
    }
    
    payload <- list(
      match_date = as.character(input$match_date),
      home_team = input$home_team,
      away_team = input$away_team,
      referee = input$referee,
      score = input$score,
      youtube_url = input$youtube_url
    )
    
    url <- paste0(SUPABASE_URL, "/rest/v1/matches?id=eq.", row_id)
    
    res <- httr::PATCH(
      url,
      add_headers(
        "apikey" = SUPABASE_KEY,
        "Authorization" = paste("Bearer", SUPABASE_KEY),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(payload, auto_unbox = TRUE),
      encode = "raw"
    )
    
    if (httr::status_code(res) %in% c(200, 204)) {
      showNotification("Match updated!", type = "message")
      removeModal()
      load_matches_data()
    } else {
      showNotification("Failed to update match.", type = "error")
    }
  })
  
  
  # ▼▼ VIDEO PREVIEW FOR EDIT / CREATE MODALS ▼▼
  observeEvent(input$preview_video, {
    req(authenticated())
    req(app_state() == "matches")
    
    url <- input$youtube_url
    embed_url <- NULL
    
    if (!is.null(url) && url != "") {
      video_id <- extract_youtube_id(url)
      if (!is.null(video_id)) {
        embed_url <- paste0("https://www.youtube.com/embed/", video_id)
      }
    }
    
    if (!is.null(embed_url)) {
      video_embed_url(embed_url)
      showNotification("Video loaded successfully!", type = "message")
    } else {
      showNotification("Invalid YouTube URL.", type = "error")
    }
  })
  
  output$video_preview <- renderUI({
    req(app_state() == "matches")
    url <- video_embed_url()
    
    if (!is.null(url)) {
      div(
        class = "video-container",
        tags$iframe(
          src = url,
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = TRUE
        )
      )
    }
  })
  
  
  # ▼▼ CREATE NEW MATCH MODAL (unchanged except 3-dots work now) ▼▼
  observeEvent(input$upload, {
    req(authenticated())
    req(app_state() == "matches")
    
    video_embed_url(NULL)
    
    showModal(modalDialog(
      title = "Match Details",
      size = "l",
      
      textInput("youtube_url", "YouTube Video Link:",
                placeholder = "Paste YouTube video URL"),
      
      actionButton("preview_video", "Preview Video", class = "btn-info btn-sm"),
      uiOutput("video_preview"),
      
      hr(style = "border-color: rgba(255, 255, 255, 0.1);"),
      
      textInput("home_team", "Home Team:"),
      textInput("away_team", "Away Team:"),
      textInput("referee", "Referee:"),
      textInput("score", "Score:"),
      dateInput("match_date", "Match Date:", value = Sys.Date()),
      
      footer = tagList(
        actionButton("cancel_btn", "Cancel", class = "btn-cancel", `data-dismiss` = "modal"),
        actionButton("submit_match", "Submit", class = "btn-primary")
      ),
      
      easyClose = TRUE,
      fade = TRUE
    ))
  })
  
  
  # ▼▼ SAVE NEW MATCH ▼▼
  observeEvent(input$submit_match, {
    req(authenticated())
    req(app_state() == "matches")
    req(input$youtube_url, input$home_team, input$away_team)
    req(current_user())
    
    match_record <- list(
      match_date = as.character(input$match_date),
      home_team = input$home_team,
      away_team = input$away_team,
      score = input$score,
      referee = input$referee,
      youtube_url = input$youtube_url,
      username = current_user()
    )
    
    response <- supabase_insert("matches", match_record)
    
    if (httr::status_code(response) %in% c(200, 201, 204)) {
      showNotification("Match saved successfully!", type = "message")
      removeModal()
      load_matches_data()
    } else {
      showNotification("Failed to save match to Supabase.", type = "error")
    }
  })
  
  
  # ▼▼ NEW: save edits back to Supabase via PATCH ▼▼
  observeEvent(input$save_match_edits, {
    req(authenticated())
    req(app_state() == "matches")
    
    row_id <- session$userData$edit_row_id
    if (is.null(row_id) || is.na(row_id)) {
      showNotification("Missing match ID; cannot update.", type = "error")
      return()
    }
    
    payload <- list(
      match_date = as.character(input$match_date),
      home_team = input$home_team,
      away_team = input$away_team,
      referee = input$referee,
      score = input$score,
      youtube_url = input$youtube_url
    )
    
    url <- paste0(SUPABASE_URL, "/rest/v1/matches?id=eq.", row_id)
    res <- httr::PATCH(
      url,
      add_headers(
        "apikey" = SUPABASE_KEY,
        "Authorization" = paste("Bearer", SUPABASE_KEY),
        "Content-Type" = "application/json"
      ),
      body = toJSON(payload, auto_unbox = TRUE),
      encode = "raw"
    )
    
    if (status_code(res) %in% c(200, 204)) {
      showNotification("Match updated!", type = "message")
      removeModal()
      load_matches_data()
    } else {
      showNotification("Failed to update match.", type = "error")
    }
  })
  
  # CLICK MATCH ROW -> gather info + pull EIP_Data + go to analysis
  observeEvent(input$matches_table_rows_selected, {
    req(app_state() == "matches")
    selected_row <- input$matches_table_rows_selected
    if (length(selected_row) > 0) {
      data_tbl <- matches_data()
      selected_match <- data_tbl[selected_row, ]
      # ▼ keep using HiddenURL as canonical source
      selected_url <- selected_match$HiddenURL
      home_team <- selected_match$Home
      away_team <- selected_match$Away
      
      session$userData$selected_video_url <- selected_url
      session$userData$selected_home <- home_team
      session$userData$selected_away <- away_team
      
      # NEW: load EIP_Data for this match (both directions)
      eip_rows <- supabase_select_eip_match(home_team, away_team)
      if (!is.null(eip_rows) && is.data.frame(eip_rows) && nrow(eip_rows) > 0) {
        # we will map columns to the analysis structure later in analysis section
        analysis_data(eip_rows)
      } else {
        analysis_data(NULL)
      }
      
      app_state("analysis")
    }
  })
  
  # ANALYSIS SERVER PART ----
  
  # helper to format seconds
  get_current_timer_display <- function(seconds = NULL) {
    if (is.null(seconds)) {
      seconds <- isolate(input$video_timer_seconds)
    }
    if (is.null(seconds)) return("00:00:00")
    h <- floor(seconds / 3600)
    m <- floor((seconds %% 3600) / 60)
    s <- floor(seconds %% 60)
    sprintf("%02d:%02d:%02d", h, m, s)
  }
  
  # video data
  video_data <- reactiveValues(
    video_id = NULL,
    embed_url = NULL,
    is_valid = FALSE
  )
  
  # data() will always be the analysis data visible in the table
  # start empty
  data <- reactiveVal(
    data.frame(
      ID = character(), Venue = character(), Team = character(), Opponent = character(), Half = character(),
      Direction = character(), Minute = character(), Event_Type = character(),
      Start_X = numeric(), Start_Y = numeric(), End_X = numeric(), End_Y = numeric(),
      GLS = numeric(), Linebreak = numeric(), Prior_GLS = numeric(),
      Type = character(), Pass_Receipt = character(), Source = character(),
      Defenders_Infront = character(), Tackle_Type = character(), Outcome = character(),
      Kick_Type = character(), Kick_Outcome = character(), Possession_Type = character(),
      Pen_Type = character(), Pen_Won_Type = character(), Pass_Type = character(),
      Kick_Receipt = character(), Kick_Receipt_Source = character(),
      Kick_Type_Received = character(), Catch_Pressure = character(),
      Try_Outcome = character(), Try_Assist_Type = character(),
      Thrower_ID = character(), Receiver_ID = character(), Lineout_Numbers = character(),
      Lineout_Destination = character(), Lineout_Outcome = character(), Lineout_Secondary_Outcome = character(), Contested = character(),
      Scrum_Outcome = character(), Scrum_Dominance = character(), Secondary_Outcome = character(),
      Card_Colour = character(), Player_On = character(), Player_Off = character(), Substitution_Type = character(),
      Defender_Beaten_Type = character(), Offload_Outcome = character(), Interception_Outcome = character(),
      Error = character(), Error_Outcome = character(), Turnover_Won_Outcome = character(),
      Additional_Info = character(), Clip_Start = numeric(), Clip_End = numeric(), stringsAsFactors = FALSE
    )
  )
  
  # CLICKED MATCH: when we enter analysis, load data for current logged-in user
  observe({
    req(app_state() == "analysis")
    req(current_user())
    
    home_team <- session$userData$selected_home
    away_team <- session$userData$selected_away
    logged_in_username <- current_user()
    
    # Load EIP_Data for the logged-in user only
    eip1 <- supabase_select(
      "EIP_Data",
      select_clause = "*",
      filter = list(
        username = paste0("eq.", logged_in_username),
        Team = paste0("eq.", home_team),
        Opponent = paste0("eq.", away_team)
      )
    )
    
    # Direction 2: Team = away_team AND Opponent = home_team
    eip2 <- supabase_select(
      "EIP_Data",
      select_clause = "*",
      filter = list(
        username = paste0("eq.", logged_in_username),
        Team = paste0("eq.", away_team),
        Opponent = paste0("eq.", home_team)
      )
    )
    
    # Combine both directions
    eip <- NULL
    if (!is.null(eip1) && is.data.frame(eip1) && nrow(eip1) > 0) {
      eip <- eip1
    }
    if (!is.null(eip2) && is.data.frame(eip2) && nrow(eip2) > 0) {
      if (is.null(eip)) {
        eip <- eip2
      } else {
        eip <- dplyr::bind_rows(eip, eip2)
      }
    }
    
    if (!is.null(eip) && is.data.frame(eip) && nrow(eip) > 0) {
      # SAFER MAPPING - Create a fresh template for each row
      template <- data()
      out <- template[0, ]  # Empty template with correct structure
      
      for (i in seq_len(nrow(eip))) {
        # Create new row with template structure
        new_row <- template[1, ]
        new_row[1, ] <- NA  # Clear values
        
        # Map only columns that exist in BOTH
        for (col_name in names(template)) {
          if (col_name %in% names(eip)) {
            val <- eip[[col_name]][i]
            # ✅ CRITICAL: Ensure data type compatibility
            if (is.numeric(template[[col_name]])) {
              new_row[[col_name]] <- as.numeric(val)
            } else {
              new_row[[col_name]] <- as.character(val)
            }
          }
        }
        
        out <- rbind(out, new_row, stringsAsFactors = FALSE)
      }
      
      # Ensure all numeric columns are actually numeric
      numeric_cols <- c("Start_X", "Start_Y", "End_X", "End_Y", "GLS", "Linebreak", 
                        "Prior_GLS", "Clip_Start", "Clip_End")
      for (col in numeric_cols) {
        if (col %in% names(out)) {
          out[[col]] <- as.numeric(out[[col]])
        }
      }
      
      data(out)
      showNotification(sprintf("Loaded %d events for your match", nrow(eip)), type = "message")
    } else {
      # Start with empty template
      data(data()[0, ])
      showNotification("No existing data found for this match", type = "message")
    }
  })
  
  # video player render WITH next/previous clip buttons ----
  output$video_player <- renderUI({
    req(app_state() == "analysis")
    url <- session$userData$selected_video_url
    if (is.null(url) || url == "") {
      return(
        div(
          style = "text-align: center; padding: 50px; color: #6c757d;", 
          icon("video", style = "
               font-size: 48px;
               margin-bottom: 20px;"), 
          br(), 
          "YouTube video player will appear here when a video is loaded."
        )
      )
    }
    video_id <- extract_youtube_id(url)
    if (is.null(video_id)) {
      return(tags$p("Invalid YouTube URL."))
    }
    
    video_data$video_id <- video_id
    video_data$embed_url <- paste0(
      "https://www.youtube.com/embed/", video_id,
      "?enablejsapi=1&rel=0&modestbranding=1&showinfo=0&controls=1&fs=0&iv_load_policy=3&disablekb=0")
    video_data$is_valid <- TRUE
    
    # render iframe + initialise JS player once iframe loads
    tagList(
      div(
        style = "position: relative; width: 100%;",
        tags$iframe(
          id = "youtube-player",
          width = "100%",
          height = "700",
          src = video_data$embed_url,
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = TRUE
        ),
        # NEW: clip navigation buttons
        div(
          style = "
            display: flex;
            justify-content: center;
            align-items: center;
            margin-top: 15px;
            gap: 20px;
          ",
          actionButton(
            "previous_clip", "⏮ Previous Clip",
            style = "
              background-color: transparent;
              border: 1px solid #28a745;
              color: white;
              border-radius: 8px;
              padding: 8px 20px;
            "
          ),
          actionButton(
            "next_clip", "Next Clip ⏭",
            style = "
              background-color: transparent;
              border: 1px solid #28a745;
              color: white;
              border-radius: 8px;
              padding: 8px 20px;
            "
          )
        )
      ),
      tags$script(HTML(sprintf("
      // ensure player initialises after iframe loads
      setTimeout(function() {
        if (typeof window.createPlayer === 'function') {
          window.createPlayer('%s');
          console.log('YouTube player initialised for %s');
        } else {
          console.warn('createPlayer not yet defined.');
        }
      }, 800);
    ", video_id, video_id)))
    )
  })
  
  
  # pitch
  output$pitch <- renderPlot({
    req(app_state() == "analysis")
    draw_pitch()
  })
  
  # click state
  click_state <- reactiveValues(
    awaiting_second_click = FALSE,
    event_type = NULL,
    start_x = NULL,
    start_y = NULL
  )
  
  # clip playlist
  clip_playlist <- reactiveValues(
    clips = NULL,
    clip_ends = NULL,
    current_index = 0,
    player_id = NULL,
    event_type = NULL,
    auto_advance = TRUE
  )
  
  # log event on pitch
  observeEvent(input$pitch_click, {
    req(app_state() == "analysis")
    req(input$event_type)
    click <- input$pitch_click
    x <- round(click$x, 2)
    y <- round(click$y, 2)
    is_two_click <- input$event_type %in% c("Pass", "Carry", "Kick", "Lineout")
    if (is_two_click && !click_state$awaiting_second_click) {
      click_state$awaiting_second_click <- TRUE
      click_state$event_type <- input$event_type
      click_state$start_x <- x
      click_state$start_y <- y
      showNotification("Click again to set the end point", type = "message")
    } else {
      event_type <- if (is_two_click) click_state$event_type else input$event_type
      start_x <- if (is_two_click) click_state$start_x else x
      start_y <- if (is_two_click) click_state$start_y else y
      end_x <- if (is_two_click) x else NA
      end_y <- if (is_two_click) y else NA
      
      current_time <- isolate(input$video_timer_seconds)
      if (is.null(current_time)) current_time <- 0
      clip_start <- max(0, current_time - 3)
      clip_end <- current_time + 15
      
      # Determine the team name for this event
      event_team <- ifelse(input$team != "", input$team, session$userData$selected_home)
      
      # Lookup username from matches table based on team name
      match_username <- NULL
      home_team <- session$userData$selected_home
      away_team <- session$userData$selected_away
      
      # Query matches table for this match
      match_record <- supabase_select(
        "matches",
        select_clause = "username,home_team,away_team",
        filter = list(
          home_team = paste0("eq.", home_team),
          away_team = paste0("eq.", away_team)
        )
      )
      
      # If found, use that username
      if (!is.null(match_record) && is.data.frame(match_record) && nrow(match_record) > 0) {
        match_username <- match_record$username[1]
      } else {
        # Fallback: try to derive username from team name
        match_username <- dplyr::case_when(
          grepl("Billingham", event_team, ignore.case = TRUE) ~ "Billingham_Analyst",
          grepl("Anselmians", event_team, ignore.case = TRUE) ~ "Anselmians_Analyst",
          grepl("HSFC", event_team, ignore.case = TRUE) ~ "HSFC_Analyst",
          grepl("Hartlepool", event_team, ignore.case = TRUE) ~ "Hartlepool_Analyst",
          grepl("Burton", event_team, ignore.case = TRUE) ~ "Burton_Analyst",
          TRUE ~ current_user() # Default to current logged-in user
        )
      }
      
      # Wrap entire row creation and insertion in tryCatch
      tryCatch({
        # Get the template structure from existing data
        current_data <- data()
        
        # Create new row matching the EXACT structure of current_data
        new_row <- current_data[1, ]  # Copy structure
        new_row[1, ] <- NA  # Clear all values
        
        # Now fill in the values
        new_row$ID <- as.character(input$id_number)
        new_row$Venue <- as.character(input$venue)
        new_row$Team <- as.character(event_team)
        new_row$Opponent <- as.character(ifelse(input$opponent != "", input$opponent, session$userData$selected_away))
        new_row$Half <- as.character(input$half)
        new_row$Direction <- as.character(input$direction)
        new_row$Minute <- as.character(get_current_timer_display(current_time))
        new_row$Event_Type <- as.character(event_type)
        new_row$Start_X <- as.numeric(start_x)
        new_row$Start_Y <- as.numeric(start_y)
        new_row$End_X <- as.numeric(end_x)
        new_row$End_Y <- as.numeric(end_y)
        new_row$GLS <- as.numeric(if (event_type == "Carry") input$gls else NA)
        new_row$Linebreak <- as.numeric(if (event_type == "Carry") input$linebreak else NA)
        new_row$Prior_GLS <- as.numeric(if (event_type == "Carry") input$prior_gls else NA)
        new_row$Type <- as.character(if (event_type == "Carry") input$type else NA)
        new_row$Pass_Receipt <- as.character(if (event_type == "Carry") input$pass_receipt else NA)
        new_row$Source <- as.character(if (event_type == "Carry") input$source else NA)
        new_row$Defenders_Infront <- as.character(if (event_type == "Carry") input$defenders_infront else NA)
        new_row$Tackle_Type <- as.character(if (event_type == "Tackle") input$tackle_type else NA)
        new_row$Outcome <- as.character(if (event_type == "Tackle") input$outcome else NA)
        new_row$Kick_Type <- as.character(if (event_type == "Kick") input$kick_type else NA)
        new_row$Kick_Outcome <- as.character(if (event_type == "Kick") input$kick_outcome else NA)
        new_row$Possession_Type <- as.character(if (event_type == "Possession") input$Possession_type else NA)
        new_row$Pen_Type <- as.character(if (event_type == "Pen Conceded") input$pen_type else NA)
        new_row$Pen_Won_Type <- as.character(if (event_type == "Pen Won") input$pen_won_type else NA)
        new_row$Pass_Type <- as.character(if (event_type == "Pass") input$pass_type else NA)
        new_row$Kick_Receipt <- as.character(if (event_type == "Kick Received") input$kick_received else NA)
        new_row$Kick_Receipt_Source <- as.character(if (event_type == "Kick Received") input$kick_source else NA)
        new_row$Kick_Type_Received <- as.character(if (event_type == "Kick Received") input$kick_type_received else NA)
        new_row$Catch_Pressure <- as.character(if (event_type == "Kick Received") input$catch_pressure else NA)
        new_row$Try_Outcome <- as.character(if (event_type == "Try") input$try else NA)
        new_row$Try_Assist_Type <- as.character(if (event_type == "Try Assist") input$try_assist else NA)
        new_row$Thrower_ID <- as.character(if (event_type == "Lineout") input$thrower else NA)
        new_row$Receiver_ID <- as.character(if (event_type == "Lineout") input$receiver else NA)
        new_row$Lineout_Numbers <- as.character(if (event_type == "Lineout") input$numbers else NA)
        new_row$Lineout_Destination <- as.character(if (event_type == "Lineout") input$lineout_destination else NA)
        new_row$Lineout_Outcome <- as.character(if (event_type == "Lineout") input$lineout_outcome else NA)
        new_row$Lineout_Secondary_Outcome <- as.character(if (event_type == "Lineout") input$lineout_secondary_outcome else NA)
        new_row$Contested <- as.character(if (event_type == "Lineout") input$contested else NA)
        new_row$Scrum_Outcome <- as.character(if (event_type == "Scrum") input$scrum_outcome else NA)
        new_row$Scrum_Dominance <- as.character(if (event_type == "Scrum") input$scrum_dominance else NA)
        new_row$Secondary_Outcome <- as.character(if (event_type == "Scrum") input$scrum_secondary_outcome else NA)
        new_row$Card_Colour <- as.character(if (event_type == "Card") input$card_colour else NA)
        new_row$Player_On <- as.character(if (event_type == "Substitution") input$player_on else NA)
        new_row$Player_Off <- as.character(if (event_type == "Substitution") input$player_off else NA)
        new_row$Substitution_Type <- as.character(if (event_type == "Substitution") input$sub_type else NA)
        new_row$Defender_Beaten_Type <- as.character(if (event_type == "Defender Beaten") input$defender_beaten_type else NA)
        new_row$Offload_Outcome <- as.character(if (event_type == "Offload") input$offload else NA)
        new_row$Interception_Outcome <- as.character(if (event_type == "Interception") input$interception else NA)
        new_row$Error <- as.character(if (event_type == "Error") input$error else NA)
        new_row$Error_Outcome <- as.character(if (event_type == "Error") input$error_outcome else NA)
        new_row$Turnover_Won_Outcome <- as.character(if (event_type == "Turnover Won") input$turnover_won_outcome else NA)
        new_row$Additional_Info <- as.character(input$additional_info)
        new_row$Clip_Start <- as.numeric(clip_start)
        new_row$Clip_End <- as.numeric(clip_end)
        
        # Now rbind should work because columns match exactly
        data(rbind(current_data, new_row, stringsAsFactors = FALSE))
        
        # ALSO update analysis_data to keep them in sync
        current_analysis <- analysis_data()
        if (!is.null(current_analysis) && is.data.frame(current_analysis) && nrow(current_analysis) > 0) {
          # Use same template approach for analysis_data
          analysis_new_row <- current_analysis[1, ]
          analysis_new_row[1, ] <- NA
          # Copy values from new_row to analysis_new_row
          for (col in names(analysis_new_row)) {
            if (col %in% names(new_row)) {
              analysis_new_row[[col]] <- new_row[[col]]
            }
          }
          analysis_data(rbind(current_analysis, analysis_new_row, stringsAsFactors = FALSE))
        } else {
          analysis_data(new_row)
        }
        
        # also push to Supabase EIP_Data (so table is auto-updated)
        eip_payload <- as.list(new_row[1, ])
        # make sure Team/Opponent are set (fall back again)
        if (is.null(eip_payload$Team) || is.na(eip_payload$Team) || eip_payload$Team == "") {
          eip_payload$Team <- session$userData$selected_home
        }
        if (is.null(eip_payload$Opponent) || is.na(eip_payload$Opponent) || eip_payload$Opponent == "") {
          eip_payload$Opponent <- session$userData$selected_away
        }
        
        # Use the looked-up username
        eip_payload$username <- match_username
        
        resp <- supabase_insert_eip_event(eip_payload)
        if (!status_code(resp) %in% c(200, 201, 204)) {
          showNotification("Event logged locally but could not sync to database", type = "warning")
        } else {
          showNotification("Event logged successfully!", type = "message")
          
          # ✅ CAPTURE VALUES OUTSIDE OF later() - before entering non-reactive context
          home_team_val <- session$userData$selected_home
          away_team_val <- session$userData$selected_away
          logged_in_username_val <- current_user()
          current_template <- isolate(data())  # ✅ Use isolate() to access reactive value
          
          # ✅ RELOAD DATA FROM SUPABASE TO ENSURE SYNC
          # Small delay to ensure Supabase has processed the insert
          later(function() {
            # Load EIP_Data for the logged-in user only
            eip1 <- supabase_select(
              "EIP_Data",
              select_clause = "*",
              filter = list(
                username = paste0("eq.", logged_in_username_val),
                Team = paste0("eq.", home_team_val),
                Opponent = paste0("eq.", away_team_val)
              )
            )
            
            eip2 <- supabase_select(
              "EIP_Data",
              select_clause = "*",
              filter = list(
                username = paste0("eq.", logged_in_username_val),
                Team = paste0("eq.", away_team_val),
                Opponent = paste0("eq.", home_team_val)
              )
            )
            
            eip <- NULL
            if (!is.null(eip1) && is.data.frame(eip1) && nrow(eip1) > 0) {
              eip <- eip1
            }
            if (!is.null(eip2) && is.data.frame(eip2) && nrow(eip2) > 0) {
              if (is.null(eip)) {
                eip <- eip2
              } else {
                eip <- dplyr::bind_rows(eip, eip2)
              }
            }
            
            if (!is.null(eip) && is.data.frame(eip) && nrow(eip) > 0) {
              template <- current_template
              
              # If template is empty, create initial structure
              if (nrow(template) == 0) {
                template <- data.frame(
                  ID = character(), Venue = character(), Team = character(), Opponent = character(), Half = character(),
                  Direction = character(), Minute = character(), Event_Type = character(),
                  Start_X = numeric(), Start_Y = numeric(), End_X = numeric(), End_Y = numeric(),
                  GLS = numeric(), Linebreak = numeric(), Prior_GLS = numeric(),
                  Type = character(), Pass_Receipt = character(), Source = character(),
                  Defenders_Infront = character(), Tackle_Type = character(), Outcome = character(),
                  Kick_Type = character(), Kick_Outcome = character(), Possession_Type = character(),
                  Pen_Type = character(), Pen_Won_Type = character(), Pass_Type = character(),
                  Kick_Receipt = character(), Kick_Receipt_Source = character(),
                  Kick_Type_Received = character(), Catch_Pressure = character(),
                  Try_Outcome = character(), Try_Assist_Type = character(),
                  Thrower_ID = character(), Receiver_ID = character(), Lineout_Numbers = character(),
                  Lineout_Destination = character(), Lineout_Outcome = character(), Lineout_Secondary_Outcome = character(), Contested = character(),
                  Scrum_Outcome = character(), Scrum_Dominance = character(), Secondary_Outcome = character(),
                  Card_Colour = character(), Player_On = character(), Player_Off = character(), Substitution_Type = character(),
                  Defender_Beaten_Type = character(), Offload_Outcome = character(), Interception_Outcome = character(),
                  Error = character(), Error_Outcome = character(), Turnover_Won_Outcome = character(),
                  Additional_Info = character(), Clip_Start = numeric(), Clip_End = numeric(), stringsAsFactors = FALSE
                )
              }
              
              out <- template[0, ]
              
              for (i in seq_len(nrow(eip))) {
                new_row_reload <- template[1:1, ]
                new_row_reload[1, ] <- NA
                
                for (col_name in names(template)) {
                  if (col_name %in% names(eip)) {
                    val <- eip[[col_name]][i]
                    if (is.numeric(template[[col_name]])) {
                      new_row_reload[[col_name]] <- as.numeric(val)
                    } else {
                      new_row_reload[[col_name]] <- as.character(val)
                    }
                  }
                }
                
                out <- rbind(out, new_row_reload, stringsAsFactors = FALSE)
              }
              
              numeric_cols <- c("Start_X", "Start_Y", "End_X", "End_Y", "GLS", "Linebreak", 
                                "Prior_GLS", "Clip_Start", "Clip_End")
              for (col in numeric_cols) {
                if (col %in% names(out)) {
                  out[[col]] <- as.numeric(out[[col]])
                }
              }
              
              # ✅ Update reactive values (this is safe from inside later())
              data(out)
              analysis_data(out)
              cat("Reloaded", nrow(out), "events from Supabase\n")
            }
          }, 0.5)  # 500ms delay
        }
        
        # Second clicks ----
        click_state$awaiting_second_click <- FALSE
        click_state$event_type <- NULL
        click_state$start_x <- NULL
        click_state$start_y <- NULL
        
      }, error = function(e) {
        # Detailed error logging
        cat("\n=== ERROR IN PITCH CLICK ===\n")
        cat("Error message:", e$message, "\n")
        cat("Event type:", event_type, "\n")
        cat("Player ID:", input$id_number, "\n")
        
        # Show structure of current data
        cat("\nCurrent data columns:", ncol(data()), "\n")
        cat("Current data column names:\n")
        print(names(data()))
        
        # Show structure of new row
        if (exists("new_row")) {
          cat("\nNew row columns:", ncol(new_row), "\n")
          cat("New row column names:\n")
          print(names(new_row))
        }
        
        cat("=== END ERROR LOG ===\n\n")
        
        # Reset click state even on error
        click_state$awaiting_second_click <- FALSE
        click_state$event_type <- NULL
        click_state$start_x <- NULL
        click_state$start_y <- NULL
        
        # Show user-friendly error
        showNotification(
          paste("Error logging event:", e$message), 
          type = "error", 
          duration = 10
        )
      })
    }
  })
  
  # data table
  output$dataTable <- renderDT({
    req(app_state() == "analysis")
    df <- data()
    datatable(
      df, 
      options = list(
        pageLength = 20,
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'Bfrtip',
        ordering = TRUE,
        initComplete = JS("
    // Adjust columns once the table is ready
    setTimeout(function() {
      $($.fn.dataTable.tables(true)).DataTable().columns.adjust().draw(false);
    }, 300);
  "),
        drawCallback = JS("
    // Ensure header stays aligned on each redraw
    $($.fn.dataTable.tables(true)).DataTable().columns.adjust();
  ")
      ),
      selection = 'single',
      editable = TRUE
    )
  })
  
  
  # summary table (original logic: player IDs across top)
  output$summaryTable <- renderDT({
    req(app_state() == "analysis")
    df <- data()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No data logged yet"), 
                       options = list(dom = 't'),
                       rownames = FALSE))
    }
    tryCatch({
      event_types <- unique(df$Event_Type)
      event_types <- event_types[!is.na(event_types) & event_types != ""]
      player_ids <- unique(df$ID)
      player_ids <- player_ids[!is.na(player_ids) & player_ids != "" & 
                                 player_ids != "Unknown" & player_ids != "Team" & 
                                 player_ids != "Opposition"]
      if (length(event_types) == 0 || length(player_ids) == 0) {
        return(datatable(data.frame(Message = "No valid player events logged yet"),
                         options = list(dom = 't'),
                         rownames = FALSE))
      }
      event_types <- sort(event_types)
      player_ids <- sort(as.character(player_ids))
      summary_matrix <- matrix(0, nrow = length(event_types), ncol = length(player_ids))
      rownames(summary_matrix) <- event_types
      colnames(summary_matrix) <- player_ids
      for (i in 1:nrow(df)) {
        event <- df$Event_Type[i]
        player <- as.character(df$ID[i])
        if (!is.na(event) && !is.na(player) && 
            event %in% event_types && player %in% player_ids) {
          summary_matrix[event, player] <- summary_matrix[event, player] + 1
        }
      }
      summary_df <- as.data.frame(summary_matrix, stringsAsFactors = FALSE)
      summary_df <- cbind(Event_Type = rownames(summary_df), summary_df, stringsAsFactors = FALSE)
      rownames(summary_df) <- NULL
      
      datatable(
        summary_df,
        options = list(
          pageLength = 20,
          autoWidth = TRUE,
          scrollX = TRUE,
          dom = 'Bfrtip',
          ordering = TRUE
        ),
        selection = 'none',
        escape = FALSE,
        rownames = FALSE,
        callback = JS("
      table.on('click', 'td', function() {
        var cell = table.cell(this);
        var colIdx = cell.index().column;
        var rowIdx = cell.index().row;
        if (colIdx === 0) return;
        var count = parseInt(cell.data());
        if (count === 0 || isNaN(count)) return;
        var playerId = table.column(colIdx).header().textContent;
        var eventType = table.cell(rowIdx, 0).data();
        Shiny.setInputValue('summary_cell_clicked', {
          player: playerId,
          event: eventType,
          count: count,
          timestamp: Date.now()
        }, {priority: 'event'});
      });
    ")
      ) %>%
        formatStyle(
          columns = 2:ncol(summary_df),
          cursor = 'pointer',
          backgroundColor = styleInterval(
            c(0.5, 1, 3, 5, 10),
            c('transparent', 'springgreen1', 'springgreen2', 'springgreen3', 'springgreen4', 'darkgreen')
          )
        )
    }, error = function(e) {
      return(datatable(data.frame(Error = paste("Error creating summary:", e$message)),
                       options = list(dom = 't'),
                       rownames = FALSE))
    })
  })
  
  # summary clicks -> play clips
  observeEvent(input$summary_cell_clicked, {
    req(app_state() == "analysis")
    req(video_data$is_valid)
    player_id <- input$summary_cell_clicked$player
    event_type <- input$summary_cell_clicked$event
    df <- data()
    filtered_df <- df[df$ID == player_id & df$Event_Type == event_type & 
                        !is.na(df$Clip_Start), ]
    if (nrow(filtered_df) == 0) {
      showNotification("No clips available for this combination", type = "warning")
      return()
    }
    filtered_df <- filtered_df[order(filtered_df$Clip_Start), ]
    clip_playlist$clips <- filtered_df$Clip_Start
    clip_playlist$clip_ends <- filtered_df$Clip_End
    clip_playlist$current_index <- 1
    clip_playlist$player_id <- player_id
    clip_playlist$event_type <- event_type
    clip_playlist$auto_advance <- TRUE
    
    clip_start <- filtered_df$Clip_Start[1]
    runjs(sprintf("
    if (typeof player !== 'undefined' && player && player.seekTo) {
      player.seekTo(%f, true);
      player.playVideo();
    } else {
      console.warn('Player not ready, retrying...');
      setTimeout(function() {
        if (typeof player !== 'undefined' && player.seekTo) {
          player.seekTo(%f, true);
          player.playVideo();
        }
      }, 1500);
    }
  ", clip_start, clip_start))
    
    showNotification(
      sprintf("Playing clip 1 of %d for Player %s - %s", 
              nrow(filtered_df), player_id, event_type),
      type = "message",
      duration = 3
    )
  })
  
  
  # auto-advance
  observe({
    req(app_state() == "analysis")
    req(clip_playlist$clips)
    req(clip_playlist$auto_advance)
    req(input$video_timer_seconds)
    if (clip_playlist$current_index > 0 && 
        clip_playlist$current_index <= length(clip_playlist$clips)) {
      current_clip_end <- clip_playlist$clip_ends[clip_playlist$current_index]
      if (input$video_timer_seconds >= (current_clip_end - 0.5) && 
          input$video_timer_seconds < (current_clip_end + 2)) {
        if (clip_playlist$current_index < length(clip_playlist$clips)) {
          clip_playlist$current_index <- clip_playlist$current_index + 1
          next_clip_start <- clip_playlist$clips[clip_playlist$current_index]
          if (!is.null(session$userData$current_clips)) {
            session$userData$current_clips$current <- clip_playlist$current_index
          }
          runjs(sprintf("if (playerReady && player) { player.seekTo(%f, true); player.playVideo(); }", next_clip_start))
          showNotification(
            sprintf("Playing clip %d of %d", 
                    clip_playlist$current_index, 
                    length(clip_playlist$clips)),
            type = "message",
            duration = 2
          )
          clip_playlist$auto_advance <- FALSE
          later(function() {
            clip_playlist$auto_advance <- TRUE
          }, 0.1)
        } else {
          clip_playlist$auto_advance <- FALSE
          showNotification("Playlist complete", type = "message", duration = 2)
        }
      }
    }
  })
  
  # manual clip navigation buttons ----
  observeEvent(input$next_clip, {
    req(app_state() == "analysis")
    req(clip_playlist$clips)
    
    if (clip_playlist$current_index < length(clip_playlist$clips)) {
      clip_playlist$current_index <- clip_playlist$current_index + 1
      clip_start <- clip_playlist$clips[clip_playlist$current_index]
      
      runjs(sprintf("
      if (typeof player !== 'undefined' && player && player.seekTo) {
        player.pauseVideo();
        setTimeout(function() {
          player.seekTo(%f, true);
          player.playVideo();
        }, 200);
      } else {
        console.warn('Player not ready or seekTo unavailable.');
      }
    ", clip_start))
      
      showNotification(
        sprintf('Playing clip %d of %d', clip_playlist$current_index, length(clip_playlist$clips)),
        type = 'message', duration = 2
      )
    } else {
      showNotification('No more clips.', type = 'warning')
    }
  })
  
  
  
  observeEvent(input$previous_clip, {
    req(app_state() == "analysis")
    req(clip_playlist$clips)
    if (clip_playlist$current_index > 1) {
      clip_playlist$current_index <- clip_playlist$current_index - 1
      clip_start <- clip_playlist$clips[clip_playlist$current_index]
      runjs(sprintf("if (playerReady && player) { player.seekTo(%f, true); player.playVideo(); }", clip_start))
      showNotification(
        sprintf("Playing clip %d of %d", clip_playlist$current_index, length(clip_playlist$clips)),
        type = "message", duration = 2
      )
    } else {
      showNotification("Already at first clip.", type = "warning")
    }
  })
  
  # edit cells
  observeEvent(input$dataTable_cell_edit, {
    req(app_state() == "analysis")
    info <- input$dataTable_cell_edit
    df <- data()
    df[info$row, info$col] <- info$value
    data(df)
    showNotification("Cell updated successfully", type = "message")
  })
  
  # delete row
  observeEvent(input$delete_row, {
    req(app_state() == "analysis")
    selected <- input$dataTable_rows_selected
    if (length(selected)) {
      df <- data()
      df <- df[-selected, ]
      data(df)
      showNotification("Selected row deleted", type = "message")
    } else {
      showNotification("Please select a row to delete", type = "error")
    }
  })
  
  # click row -> play clip
  observeEvent(input$dataTable_rows_selected, {
    req(app_state() == "analysis")
    selected <- input$dataTable_rows_selected
    if (length(selected) && video_data$is_valid) {
      df <- data()
      clip_start <- df[selected, "Clip_Start"]
      if (!is.na(clip_start) && !is.null(clip_start)) {
        runjs(sprintf("
        if (typeof player !== 'undefined' && player && player.seekTo) {
          player.seekTo(%f, true);
          player.playVideo();
        } else {
          console.warn('Player not ready, retrying...');
          setTimeout(function() {
            if (typeof player !== 'undefined' && player.seekTo) {
              player.seekTo(%f, true);
              player.playVideo();
            }
          }, 1500);
        }
      ", clip_start, clip_start))
        showNotification(sprintf("Playing clip from %s", df[selected, "Minute"]), type = "message", duration = 3)
      } else {
        showNotification("No clip timestamp available for this event", type = "warning")
      }
    }
  })
  
  
  # hotkeys
  observeEvent(input$hotkey, {
    req(app_state() == "analysis")
    updateSelectInput(session, "event_type", selected = input$hotkey)
  })
  
  observeEvent(input$hotkey_id_number, {
    req(app_state() == "analysis")
    updateSelectInput(session, "id_number", selected = as.character(input$hotkey_id_number))
  })
  
  # download
  output$download_data <- downloadHandler(
    filename = function() {
      paste("rugby_event_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  # back to matches
  observeEvent(input$back_to_matches, {
    req(app_state() == "analysis")
    app_state("matches")
  })
}



# RUN APP ----
shinyApp(ui = ui, server = server)