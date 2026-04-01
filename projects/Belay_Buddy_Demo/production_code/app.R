# Belay Buddy – Shiny app
# Run: shiny::runApp("app.R", port = 3838)  # port optional; default varies if omitted

# Project root for precomputed/ (getwd() is often wrong with RStudio, Rscript -e, or runApp from another folder).
belay_buddy_find_project_root <- function(start = getwd()) {
  start <- normalizePath(start, winslash = "/", mustWork = FALSE)
  if (is.na(start) || !nzchar(start)) start <- normalizePath(".", winslash = "/", mustWork = FALSE)
  cur <- start
  for (i in seq_len(16L)) {
    if (file.exists(file.path(cur, "app.R")) &&
        file.exists(file.path(cur, "R", "session.R"))) {
      return(cur)
    }
    parent <- normalizePath(file.path(cur, ".."), winslash = "/", mustWork = FALSE)
    if (identical(parent, cur)) break
    cur <- parent
  }
  start
}
if (is.null(getOption("belay_buddy_project_root", default = NULL))) {
  options(belay_buddy_project_root = belay_buddy_find_project_root())
}

source("R/session.R")
source("R/pairing.R")
source("R/html_export.R")

# Host UX: compute and display upcoming rounds in 5-round bursts.
BB_BURST_ROUNDS <- 5L
BB_INITIAL_SCHEDULE_ROUNDS <- BB_BURST_ROUNDS
BB_PRIMARY_SCHEDULE_ROUNDS <- BB_BURST_ROUNDS
BB_SECOND_TIER_END <- BB_BURST_ROUNDS
BB_THIRD_TIER_END <- BB_BURST_ROUNDS

bb_session_sync <- shiny::reactiveValues(stamp = 0L)
bb_register_shared_stamp(bb_session_sync)

# Modern, mobile-first theme: teal primary + readable neutrals
theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = bslib::font_google("DM Sans"),
  heading_font = bslib::font_google("DM Sans"),
  primary = "#0f766e",
  secondary = "#475569",
  success = "#059669",
  info = "#0369a1",
  "border-radius" = "10px"
)

ui <- bslib::page_fluid(
  title = "Belay Buddy",
  theme = theme,
  fillable = TRUE,
  shiny::tags$head(
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"),
    shiny::tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"),
    shiny::tags$link(rel = "stylesheet", href = "custom.css"),
    shiny::tags$script(shiny::HTML("
(function() {
  var KEY_PARTICIPANT = 'belayBuddy_recent_names_v1';
  var KEY_HOST = 'belayBuddy_recent_host_names_v1';
  function readRecent(key) {
    try { return JSON.parse(localStorage.getItem(key) || '[]'); } catch (e) { return []; }
  }
  window.bbPushRecentName = function(name, scope) {
    if (!name || !String(name).trim()) return;
    scope = scope || 'participant';
    var key = (scope === 'host') ? KEY_HOST : KEY_PARTICIPANT;
    var n = String(name).trim();
    var arr = readRecent(key).filter(function(x) { return x !== n; });
    arr.unshift(n);
    localStorage.setItem(key, JSON.stringify(arr.slice(0, 20)));
    if (typeof window.bbPaintRecentChips === 'function') window.bbPaintRecentChips();
  };
  window.bbPaintRecentChips = function() {
    var inRoom = {};
    if (Array.isArray(window.bbInRoomNamesLower)) {
      window.bbInRoomNamesLower.forEach(function(x) {
        var k = String(x).trim().toLowerCase();
        if (k) inRoom[k] = true;
      });
    }

    function paintOne(containerId, storageKey, ariaPrefix) {
      var host = document.getElementById(containerId);
      if (!host) return;
      host.textContent = '';
      readRecent(storageKey).forEach(function(n) {
        var raw = String(n).trim();
        var k = raw.toLowerCase();
        var inThisRoom = !!inRoom[k];
        var b = document.createElement('button');
        b.type = 'button';
        if (inThisRoom) {
          b.className = 'btn btn-sm bb-recent-chip bb-recent-chip-in-room';
          b.disabled = true;
          b.setAttribute('aria-label', raw + ', already in this room');
          var ic = document.createElement('i');
          ic.className = 'bi bi-check-circle-fill bb-recent-chip-check';
          ic.setAttribute('aria-hidden', 'true');
          var sp = document.createElement('span');
          sp.className = 'bb-recent-chip-text';
          sp.textContent = raw;
          b.appendChild(ic);
          b.appendChild(sp);
        } else {
          b.className = 'btn btn-sm bb-recent-chip';
          b.setAttribute('aria-label', ariaPrefix + raw);
          b.textContent = n;
          b.addEventListener('click', function() {
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('bb_quick_name_request', { n: n, t: Date.now() }, { priority: 'event' });
            }
          });
        }
        host.appendChild(b);
      });
    }

    // Participants: existing list (kept unchanged)
    paintOne('bb-recent-chips-host', KEY_PARTICIPANT, 'Add ');
    // Host join prompt: separate list (starts empty)
    paintOne('bb-recent-chips-host-only', KEY_HOST, 'Join as host ');
  };
  $(document).on('shiny:connected', function() {
    Shiny.addCustomMessageHandler('bb_push_recent', function(msg) {
      if (!msg || !msg.name) return;
      var scope = msg.scope || 'participant';
      var k = String(msg.name).trim().toLowerCase();
      if (k) {
        if (!Array.isArray(window.bbInRoomNamesLower)) window.bbInRoomNamesLower = [];
        if (window.bbInRoomNamesLower.indexOf(k) === -1) window.bbInRoomNamesLower.push(k);
      }
      window.bbPushRecentName(msg.name, scope);
    });
    setTimeout(window.bbPaintRecentChips, 100);
  });
})();
    ")),
    shiny::tags$script(shiny::HTML("
(function() {
  function defaultScheduleFilename() {
    var d = new Date();
    function pad(n) { return n < 10 ? '0' + n : '' + n; }
    return 'belay-buddy-schedule-' + d.getFullYear() + pad(d.getMonth() + 1) + pad(d.getDate()) + '-' +
      pad(d.getHours()) + pad(d.getMinutes()) + pad(d.getSeconds()) + '.html';
  }
  function filenameFromContentDisposition(cd) {
    if (!cd) return null;
    var m = cd.match(/filename\\*=UTF-8''([^;]+)/i);
    if (m) {
      try { return decodeURIComponent(m[1].trim().replace(/^[\"']|[\"']$/g, '')); } catch (e) { return m[1].trim(); }
    }
    m = cd.match(/filename=(.+)/i);
    if (!m) return null;
    var raw = m[1].trim();
    if (raw.charCodeAt(0) === 34) {
      var end = raw.indexOf('\\x22', 1);
      if (end > 0) raw = raw.slice(1, end);
    } else {
      raw = raw.split(';')[0].replace(/^[\"']|[\"']$/g, '');
    }
    return raw || null;
  }
  window.bbShareScheduleHtml = function() {
    (async function() {
      var a = document.getElementById('download_schedule_html');
      var href = a && a.getAttribute('href');
      if (!a || !href || href === '#' || href.indexOf('javascript:') === 0) {
        window.alert('The schedule file is not ready yet. Wait a moment and try again.');
        return;
      }
      var url = a.href;
      try {
        var res = await fetch(url, { credentials: 'same-origin' });
        if (!res.ok) throw new Error('Could not load the schedule (' + res.status + ').');
        var blob = await res.blob();
        var fname = filenameFromContentDisposition(res.headers.get('Content-Disposition')) || defaultScheduleFilename();
        var file = new File([blob], fname, { type: 'text/html' });
        if (!navigator.share) {
          window.alert('Sharing is not available in this browser. Use Download, then attach the file from your files app (e.g. WhatsApp → attach → document).');
          return;
        }
        var payload = { title: 'Belay Buddy schedule', text: 'Offline climbing schedule (HTML)', files: [file] };
        if (navigator.canShare && !navigator.canShare(payload)) {
          window.alert('This browser cannot hand off the file to other apps. Use Download, then share the saved file from your device.');
          return;
        }
        await navigator.share(payload);
      } catch (e) {
        if (e && e.name === 'AbortError') return;
        window.alert((e && e.message) ? e.message : 'Could not share the schedule.');
      }
    })();
  };
})();
    "))
  ),
  bslib::navset_card_underline(
    id = "main",
    title = shiny::tags$span(shiny::tags$strong("Belay Buddy")),
    # ---- Landing ----
    bslib::nav_panel(
      "Start",
      shiny::tags$p(
        class = "text-muted mb-3",
        style = "font-size: 0.95rem;",
        "Belay rotation planner. Create a session, add people, then start."
      ),
      bslib::card(
        class = "bb-card bb-landing-card mb-2",
        bslib::card_header(class = "bb-landing-head", "Start a new session"),
        bslib::card_body(
          shiny::actionButton("create_btn", "Create session", class = "btn-success btn-cta-mobile")
        )
      ),
      shiny::uiOutput("host_join_ui")
    ),
    # ---- Session (shown when in a session) ----
    bslib::nav_panel(
      "Session",
      shiny::tags$div(
        class = "d-none",
        shiny::textInput("bb_has_session", label = NULL, value = "0"),
        shiny::textInput("bb_show_add_panel", label = NULL, value = "1")
      ),
      shiny::conditionalPanel(
        condition = "input.bb_has_session == '1' && input.bb_show_add_panel == '1'",
        bslib::card(
          class = "bb-card bb-add-people-card mb-2",
          bslib::card_header(
            class = "bb-card-header-accent",
            shiny::tags$i(class = "bi bi-person-plus-fill me-2", `aria-hidden` = "true"),
            "Add people from this device"
          ),
          bslib::card_body(
            class = "bb-card-body-tight",
            shiny::tags$p(
              class = "text-muted mb-2",
              style = "font-size: 0.82rem; line-height: 1.4;",
              "Enter a name, tap Add, or use a shortcut below. Duplicate names are not allowed."
            ),
            shiny::textInput(
              "add_person_name",
              shiny::tags$span(class = "visually-hidden", "Name to add"),
              placeholder = "e.g. Kevin",
              width = "100%"
            ),
            shiny::actionButton(
              "add_person_btn",
              shiny::tagList(shiny::tags$i(class = "bi bi-plus-lg me-2", `aria-hidden` = "true"), "Add to session"),
              class = "btn-primary btn-cta-mobile mt-1",
              width = "100%"
            ),
            shiny::tags$p(class = "bb-recent-label text-muted mb-1 mt-2", "Recently used on this device"),
            shiny::tags$div(
              id = "bb-recent-chips-host",
              class = "bb-recent-chips-host",
              role = "group",
              `aria-label` = "Recently used names"
            ),
            shiny::tags$p(
              class = "text-muted mb-0 mt-2",
              style = "font-size: 0.75rem; line-height: 1.35;",
              shiny::tags$strong("Tip:"), " adding or removing participants will apply starting the next round."
            )
          )
        )
      ),
      shiny::tags$div(
        class = "bb-hidden-dl",
        shiny::downloadButton("download_schedule_html", label = "Save schedule HTML", class = "btn btn-secondary btn-sm")
      ),
      shiny::uiOutput("schedule_backup_ui"),
      shiny::uiOutput("session_ui")
    )
  )
)

server <- function(input, output, session) {
  # In-session identity (set when user joins)
  session$userData$room_code <- NULL
  session$userData$participant_id <- NULL
  # After "Create", we show host-join form with this code
  created_room_code <- shiny::reactiveVal(NULL)
  session$userData$last_share_prompt_round <- NA_integer_

  # ---- Create session ----
  shiny::observeEvent(input$create_btn, {
    code <- create_session()
    created_room_code(code)
  })

  # Host join form (after creating)
  output$host_join_ui <- shiny::renderUI({
    code <- created_room_code()
    if (is.null(code)) return(NULL)
    bslib::card(
      class = "bb-card mt-3",
      bslib::card_header("You created a session"),
      bslib::card_body(
        shiny::textInput("host_name", "Your name to join as host", placeholder = "e.g. Shane", width = "100%"),
        shiny::tags$p(
          class = "bb-recent-label text-muted mb-1 mt-2",
          "Recently used on this device"
        ),
        shiny::tags$div(
          id = "bb-recent-chips-host-only",
          class = "bb-recent-chips-host",
          role = "group",
          `aria-label` = "Recently used host names"
        ),
        shiny::tags$script(
          shiny::HTML("if (typeof window.bbPaintRecentChips === 'function') setTimeout(window.bbPaintRecentChips, 0);")
        ),
        shiny::actionButton("host_join_btn", "Join session", class = "btn-primary btn-cta-mobile", style = "margin-top: 0.5rem;")
      )
    )
  })

  shiny::observeEvent(input$host_join_btn, {
    code <- created_room_code()
    name <- trimws(input$host_name)
    if (!nzchar(name)) return()
    if (isTRUE(session_display_name_taken(code, name))) {
      shiny::showNotification(
        paste0("Someone named \"", name, "\" is already in this room."),
        type = "warning",
        duration = 5
      )
      return()
    }
    pid <- join_session(code, name, "top_rope")
    if (is.null(pid)) return()
    session$userData$room_code <- code
    session$userData$participant_id <- pid
    created_room_code(NULL)
    shiny::updateTextInput(session, "bb_has_session", value = "1")
    session$sendCustomMessage("bb_push_recent", list(name = name, scope = "host"))
    bslib::nav_select(id = "main", selected = "Session")
  })

  # Join-by-code flow removed from UI (host-only workflow).

  # Shared stamp bumps on every server mutation - invalidates this reactive in EVERY open tab (reactivePoll does not).
  session_data <- shiny::reactive({
    bb_session_sync$stamp
    rc <- session$userData$room_code
    if (is.null(rc)) return(NULL)
    session_state(rc)
  })

  # Host-only workflow: always show add/remove card.
  shiny::observe({
    bb_session_sync$stamp
    rc <- shiny::isolate(session$userData$room_code)
    if (is.null(rc)) return()
    st <- session_state(rc)
    if (is.null(st)) return()
    shiny::updateTextInput(session, "bb_show_add_panel", value = "1")
  })

  try_add_person_to_room <- function(name) {
    rc <- session$userData$room_code
    if (is.null(rc)) return(invisible(NULL))
    name <- trimws(as.character(name %||% ""))
    if (!nzchar(name)) {
      shiny::showNotification("Enter a name to add.", type = "warning", duration = 3)
      return(invisible(NULL))
    }
    st <- session_state(rc)
    if (is.null(st)) return(invisible(NULL))
    started <- isTRUE(st$started %||% FALSE)
    if (isTRUE(session_display_name_taken(rc, name))) {
      shiny::showNotification(
        paste0("\"", name, "\" is already in this room."),
        type = "warning",
        duration = 4
      )
      return(invisible(NULL))
    }
    new_pid <- join_session(rc, name, "top_rope")
    if (is.null(new_pid)) {
      shiny::showNotification("Could not add that person (session missing).", type = "error", duration = 4)
      return(invisible(NULL))
    }
    shiny::updateTextInput(session, "add_person_name", value = "")
    session$sendCustomMessage("bb_push_recent", list(name = name, scope = "participant"))
    if (started) {
      shiny::showNotification(
        paste0("Added \"", name, "\". This will take effect starting the next round."),
        type = "message",
        duration = 5
      )
    }
    invisible(NULL)
  }

  shiny::observeEvent(input$add_person_btn, {
    try_add_person_to_room(input$add_person_name)
  })

  shiny::observeEvent(input$bb_quick_name_request, {
    req <- input$bb_quick_name_request
    nm <- if (is.list(req)) req$n %||% req[["n"]] else NULL
    if (is.null(nm)) return()
    # When the user is still on the "host join" card (no session yet),
    # clicking a recent name should join immediately as host.
    rc <- session$userData$room_code
    if (is.null(rc)) {
      code <- created_room_code()
      if (is.null(code)) return()
      name <- trimws(as.character(nm))
      if (!nzchar(name)) return()
      if (isTRUE(session_display_name_taken(code, name))) {
        shiny::showNotification(
          paste0("Someone named \"", name, "\" is already in this room."),
          type = "warning",
          duration = 5
        )
        return()
      }
      pid <- join_session(code, name, "top_rope")
      if (is.null(pid)) return()
      session$userData$room_code <- code
      session$userData$participant_id <- pid
      created_room_code(NULL)
      shiny::updateTextInput(session, "bb_has_session", value = "1")
      session$sendCustomMessage("bb_push_recent", list(name = name, scope = "host"))
      bslib::nav_select(id = "main", selected = "Session")
      return()
    }
    try_add_person_to_room(as.character(nm))
  }, ignoreNULL = TRUE)

  shiny::observeEvent(input$bb_remove_pid, {
    rc <- session$userData$room_code
    rid <- input$bb_remove_pid
    if (is.null(rc) || is.null(rid) || !nzchar(as.character(rid))) return()
    if (identical(as.character(rid), as.character(session$userData$participant_id))) return()
    st <- session_state(rc)
    if (is.null(st)) return()
    started <- isTRUE(st$started %||% FALSE)
    leave_session(rc, as.character(rid))
    if (started) {
      shiny::showNotification(
        "Removed participant. This will take effect starting the next round.",
        type = "message",
        duration = 5
      )
    }
  }, ignoreNULL = TRUE)

  # Ensure the upcoming-round burst is available for the host UI.
  shiny::observe({
    rc <- session$userData$room_code
    st <- session_data()
    if (is.null(rc) || is.null(st)) return()
    if (isTRUE(st$started %||% FALSE)) {
      ensure_burst(rc, burst_size = BB_BURST_ROUNDS)
    }
  })

  # ---- Session view ----
  output$session_ui <- shiny::renderUI({
    state <- session_data()
    rc <- session$userData$room_code
    pid <- session$userData$participant_id
    if (is.null(rc) || is.null(pid) || is.null(state)) {
      return(shiny::tags$div(
        class = "text-center text-muted py-5",
        shiny::tags$p("Join or create a session to see the rotation.", style = "font-size: 1rem;")
      ))
    }

    participants <- state$participants
    top_rope_ids <- top_rope_ids(participants)
    started <- state$started
    schedule_active <- isTRUE(state$schedule_active)

    # Pairings only after session has started (unless schedule mode is active)
    pairings <- list()
    display <- list()
    if (started && length(top_rope_ids) >= 1L && !schedule_active) {
      pairings <- compute_pairings(
        top_rope_ids,
        state$last_action,
        state$round,
        participants,
        state$pair_history
      )
      display <- pairings_for_display(pairings, participants)
    }

    # Participants: category selection removed; show names only.
    participant_ids <- names(participants)
    participant_ids <- participant_ids[!vapply(participant_ids, function(id) isFALSE(participants[[id]]$active), logical(1))]
    participant_ids <- participant_ids[order(tolower(vapply(participant_ids, function(id) {
      nm <- participants[[id]]$name %||% ""
      tolower(trimws(as.character(nm)))
    }, character(1))))]

    chips <- lapply(participant_ids, function(p_id) {
      p <- participants[[p_id]]
      nm <- trimws(p$name %||% "")
      chip_class <- if (p_id == pid) {
        "participant-chip you"
      } else {
        "participant-chip participant-chip-other"
      }
      chip <- shiny::tags$div(
        class = chip_class,
        if (p_id == pid) paste0(nm, " (you)") else nm
      )
      if (p_id != pid) {
        shiny::tags$div(
          class = "participant-chip-row",
          chip,
          shiny::tags$button(
            type = "button",
            class = "btn btn-link bb-participant-remove",
            `aria-label` = paste("Remove", nm, "from room"),
            onclick = paste0("Shiny.setInputValue('bb_remove_pid','", p_id, "',{priority:'event'});"),
            shiny::tags$i(class = "bi bi-x-lg")
          )
        )
      } else {
        shiny::tags$div(class = "participant-chip-row participant-chip-row-single", chip)
      }
    })

    room_names_lower <- unique(vapply(participant_ids, function(id) {
      tolower(trimws(as.character(participants[[id]]$name %||% "")))
    }, character(1)))
    room_names_lower <- room_names_lower[nzchar(room_names_lower)]
    in_room_json <- jsonlite::toJSON(room_names_lower, auto_unbox = FALSE)
    participant_board <- shiny::tags$div(
      class = "participant-board",
      shiny::tags$div(class = "participant-simple-list", do.call(shiny::tagList, chips)),
      shiny::tags$script(shiny::HTML(paste0(
        "window.bbInRoomNamesLower = ",
        as.character(in_room_json),
        ";\nsetTimeout(function(){ if(window.bbPaintRecentChips) bbPaintRecentChips(); }, 0);"
      )))
    )

    # Pairings: symbol-first layout (climb ↑ / belay ⛨ / swap ⇄) for quick scan
    make_pairing_rows <- function(display_obj) {
      lapply(seq_along(display_obj), function(i) {
        d <- display_obj[[i]]
      label <- if (d$type == "trio") "Trio" else paste0("Pair ", i)
      if (d$type == "pair") {
        # Visual: [Name1] [climb icon]  ·  [Name2] [belay icon], then swap
        content <- shiny::tags$div(
          class = "pairing-visual",
          shiny::tags$div(
            class = "pairing-slot pairing-slot-climb",
            shiny::tags$span(class = "pairing-name", d$climbs_first),
            shiny::tags$i(class = "bi bi-arrow-up-circle-fill pairing-icon pairing-icon-climb", title = "Climbs first")
          ),
          shiny::tags$span(class = "pairing-sep", "·"),
          shiny::tags$div(
            class = "pairing-slot pairing-slot-belay",
            shiny::tags$span(class = "pairing-name", d$belays_first),
            shiny::tags$i(class = "bi bi-link-45deg pairing-icon pairing-icon-belay", title = "Belays first")
          )
        )
        swap_row <- shiny::tags$div(
          class = "pairing-swap",
          shiny::tags$i(class = "bi bi-arrow-left-right pairing-icon-swap", `aria-hidden` = "true"),
          shiny::tags$span(class = "pairing-swap-text", "then swap")
        )
        shiny::tags$div(
          class = "pairing-row pairing-row-pair",
          shiny::tags$div(class = "pairing-label", label),
          shiny::tags$div(class = "pairing-pair-line", content, swap_row)
        )
      } else {
        # Trio: three slots, each [↑ name] [⛨ name] [− name]
        n1 <- d$climb_order[1]; n2 <- d$climb_order[2]; n3 <- d$climb_order[3]
        content <- shiny::tags$div(
          class = "pairing-trio",
          shiny::tags$div(class = "pairing-trio-slot", shiny::tags$i(class = "bi bi-arrow-up-circle-fill pairing-icon pairing-icon-climb"), shiny::tags$span(class = "pairing-name", n1), shiny::tags$i(class = "bi bi-link-45deg pairing-icon pairing-icon-belay"), shiny::tags$span(class = "pairing-name", n2), shiny::tags$i(class = "bi bi-pause-circle pairing-icon pairing-icon-rest"), shiny::tags$span(class = "pairing-name", n3)),
          shiny::tags$div(class = "pairing-trio-slot", shiny::tags$i(class = "bi bi-arrow-up-circle-fill pairing-icon pairing-icon-climb"), shiny::tags$span(class = "pairing-name", n2), shiny::tags$i(class = "bi bi-link-45deg pairing-icon pairing-icon-belay"), shiny::tags$span(class = "pairing-name", n3), shiny::tags$i(class = "bi bi-pause-circle pairing-icon pairing-icon-rest"), shiny::tags$span(class = "pairing-name", n1)),
          shiny::tags$div(class = "pairing-trio-slot", shiny::tags$i(class = "bi bi-arrow-up-circle-fill pairing-icon pairing-icon-climb"), shiny::tags$span(class = "pairing-name", n3), shiny::tags$i(class = "bi bi-link-45deg pairing-icon pairing-icon-belay"), shiny::tags$span(class = "pairing-name", n1), shiny::tags$i(class = "bi bi-pause-circle pairing-icon pairing-icon-rest"), shiny::tags$span(class = "pairing-name", n2))
        )
        shiny::tags$div(
          class = "pairing-row pairing-row-trio",
          shiny::tags$div(class = "pairing-label", label),
          content
        )
      }
      })
    }
    pairing_rows <- make_pairing_rows(display)

    # Start button when session not yet started
    start_ui <- if (!started) {
      bslib::card(
        class = "bb-card bb-start-card mb-1",
        bslib::card_header(class = "bb-card-header-accent bb-start-header", shiny::tags$i(class = "bi bi-play-circle-fill me-2", `aria-hidden` = "true"), "Ready to climb?"),
        bslib::card_body(
          shiny::tags$p(
            class = "bb-lead-text mb-2",
            paste0(
              "Start session generates the first ",
              BB_BURST_ROUNDS,
              " rounds. ",
              "Roster changes can be applied mid-session."              
            )
          ),
          shiny::actionButton("start_btn", "Start session", class = "btn-primary btn-lg btn-cta-mobile bb-btn-start")
        )
      )
    } else NULL

    # Session layout
    shiny::tagList(
      shiny::tags$div(class = "bb-session-stack bb-tight-stack",
      start_ui,
      # Participants (placed under Add people section in the overall page layout)
      bslib::card(
        class = "bb-card bb-participants-card mb-1",
        bslib::card_header(
          class = "bb-card-header-accent",
          shiny::tags$i(class = "bi bi-people-fill me-2", `aria-hidden` = "true"),
          paste0(
            "Participants (",
            length(top_rope_ids),
            ")"
          )
        ),
        bslib::card_body(class = "bb-card-body-tight", participant_board)
      ),
      # Round schedule (5-round burst)
      if (started && length(top_rope_ids) >= 1L) {
        if (isTRUE(state$schedule_active) && !is.null(state$schedule_pairings) && length(state$schedule_pairings) > 0L) {
          schedule_pairings <- state$schedule_pairings
          schedule_rounds <- as.integer(state$schedule_rounds %||% length(schedule_pairings))
          schedule_rounds <- max(1L, min(schedule_rounds, length(schedule_pairings)))

          make_one_round_card <- function(k) {
            burst_from <- as.integer(state$burst_from_round %||% state$round %||% 1L)
            abs_round <- burst_from + (k - 1L)
            disp_k <- pairings_for_display(schedule_pairings[[k]], participants)
            rows_k <- make_pairing_rows(disp_k)
            ac <- ((abs_round - 1L) %% 5L) + 1L
            subtitle <- if (k == 1L) "now" else "upcoming"
            bslib::card(
              class = paste0("bb-card bb-round-card bb-accent-", ac, " mb-1"),
              bslib::card_header(
                class = "bb-round-header",
                shiny::tags$span(class = paste0("bb-round-badge bb-round-badge-", ac), abs_round),
                shiny::tags$span(
                  class = "bb-round-header-text",
                  shiny::tags$span(class = "bb-round-title", paste0("Round ", abs_round)),
                  shiny::tags$span(class = "bb-round-sub", subtitle)
                )
              ),
              bslib::card_body(class = "bb-pairings-body", do.call(shiny::tagList, rows_k))
            )
          }

          primary_n <- min(as.integer(BB_BURST_ROUNDS), schedule_rounds)
          round_cards_primary <- lapply(seq_len(primary_n), make_one_round_card)

          next_round_top <- bslib::card(
            class = "bb-card bb-next-card mb-1",
            bslib::card_body(
              class = "bb-next-card-body",
              shiny::tags$p(
                class = "bb-next-hint mb-0",
                shiny::tags$i(class = "bi bi-skip-forward-fill me-2", `aria-hidden` = "true"),
                shiny::tags$strong("Next round"),
                shiny::tags$span(class = "text-muted", " Proceed to next round.")
              )
            ),
            bslib::card_footer(
              class = "session-footer bb-footer-cta",
              shiny::actionButton(
                "next_round_btn",
                shiny::tagList(shiny::tags$i(class = "bi bi-skip-forward-fill me-2", `aria-hidden` = "true"), "Next round"),
                class = "btn-primary btn-cta-mobile bb-btn-next"
              )
            )
          )

          do.call(shiny::tagList, c(list(next_round_top), round_cards_primary))
        } else {
          r <- as.integer(state$round)
          ac <- ((max(1L, r) - 1L) %% 5L) + 1L
          bslib::card(
            class = paste0("bb-card bb-round-card bb-round-live bb-accent-", ac, " mb-1"),
            bslib::card_header(
              class = "bb-round-header",
              shiny::tags$span(class = paste0("bb-round-badge bb-round-badge-", ac), r),
              shiny::tags$span(class = "bb-round-header-text", shiny::tags$span(class = "bb-round-title", paste0("Round ", r)), shiny::tags$span(class = "bb-round-sub", "now"))
            ),
            bslib::card_body(class = "bb-pairings-body", do.call(shiny::tagList, pairing_rows)),
            NULL
          )
        }
      }
      )
    )
  })

  # Start session: enable Round 1 and build upcoming burst.
  shiny::observeEvent(input$start_btn, {
    rc <- session$userData$room_code
    if (is.null(rc)) return()
    res <- start_session(rc)
    ok_burst <- isTRUE(res) && isTRUE(ensure_burst(rc, burst_size = BB_BURST_ROUNDS))
    if (!isTRUE(ok_burst)) {
      shiny::showNotification(
        "Could not start the session (need at least 2 participants).",
        type = "error",
        duration = 12
      )
      return()
    }
    shiny::showModal(shiny::modalDialog(
      title = shiny::tags$div(
        class = "d-flex align-items-center gap-2",
        shiny::tags$i(class = "bi bi-cloud-arrow-down text-primary", `aria-hidden` = "true"),
        shiny::tags$span("Save a copy?")
      ),
      shiny::tags$p(
        style = "font-size: 0.95rem; line-height: 1.45;",
        "Refreshing the page or the server sleeping can lose this room’s live data. Your browser will ask where to save a file - that is normal. The HTML opens offline in any browser."
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-primary w-100",
        style = "min-height: 2.75rem;",
        onclick = "var el=document.getElementById('download_schedule_html'); if(el) el.click();",
        shiny::tagList(
          shiny::tags$i(class = "bi bi-download me-2", `aria-hidden` = "true"),
          "Download schedule (HTML)"
        )
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-outline-primary w-100",
        style = "min-height: 2.75rem;",
        onclick = "if(typeof bbShareScheduleHtml==='function')bbShareScheduleHtml();",
        `aria-label` = "Share schedule HTML to another app",
        shiny::tagList(
          shiny::tags$i(class = "bi bi-share me-2", `aria-hidden` = "true"),
          "Share schedule (WhatsApp, etc.)"
        )
      ),
      shiny::tags$p(class = "text-muted small mb-0 mt-2", "On a phone, Share opens your system sheet so you can send the HTML file to WhatsApp or other apps. After closing this dialog, use the strip above the session."),
      easyClose = TRUE,
      footer = shiny::modalButton("Got it")
    ))
  })

  output$schedule_backup_ui <- shiny::renderUI({
    bb_session_sync$stamp
    rc <- session$userData$room_code
    if (is.null(rc)) return(NULL)
    st <- session_state(rc)
    if (is.null(st) || !isTRUE(st$schedule_active %||% FALSE)) return(NULL)
    shiny::tags$div(
      class = "bb-backup-bar mb-2",
      shiny::tags$div(
        class = "bb-backup-inner",
        shiny::tags$span(
          class = "bb-backup-text",
          shiny::tags$i(class = "bi bi-shield-check me-2", `aria-hidden` = "true"),
          "Save a copy of the next 5 rounds."
        ),
        shiny::tags$div(
          class = "bb-backup-actions",
          shiny::tags$button(
            type = "button",
            class = "btn btn-outline-primary btn-sm bb-backup-dl",
            onclick = "var el=document.getElementById('download_schedule_html'); if(el) el.click();",
            shiny::tagList(
              shiny::tags$i(class = "bi bi-download me-1", `aria-hidden` = "true"),
              "HTML"
            )
          ),
          shiny::tags$button(
            type = "button",
            class = "btn btn-outline-secondary btn-sm bb-backup-share",
            onclick = "if(typeof bbShareScheduleHtml==='function')bbShareScheduleHtml();",
            `aria-label` = "Share schedule HTML to another app",
            shiny::tagList(
              shiny::tags$i(class = "bi bi-share me-1", `aria-hidden` = "true"),
              "Share"
            )
          )
        )
      )
    )
  })

  output$download_schedule_html <- shiny::downloadHandler(
    filename = function() {
      paste0("belay-buddy-schedule-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".html")
    },
    content = function(file) {
      rc <- session$userData$room_code
      st <- if (!is.null(rc)) session_state(rc) else NULL
      html <- if (!is.null(st) && isTRUE(st$schedule_active %||% FALSE) && !is.null(st$schedule_pairings) && length(st$schedule_pairings) >= 1L) {
        n_rounds <- length(st$schedule_pairings)
        round_start <- as.integer(st$share_from_round %||% st$burst_from_round %||% st$round %||% 1L)
        build_schedule_html_export(st, rc, primary_rounds = n_rounds, second_tier_end = n_rounds, round_start = round_start)
      } else {
        "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>Belay Buddy</title></head><body><p>No schedule loaded in this session.</p></body></html>"
      }
      cat(html, file = file, sep = "")
      if (!is.null(rc)) clear_share_needed(rc)
    }
  )

  # Prompt host to re-download/share when roster changes take effect (after advancing).
  shiny::observe({
    bb_session_sync$stamp
    rc <- session$userData$room_code
    if (is.null(rc)) return()
    st <- session_state(rc)
    if (is.null(st)) return()
    if (!isTRUE(st$share_needed %||% FALSE)) return()
    from_round <- as.integer(st$share_from_round %||% st$round %||% 1L)
    last_shown <- session$userData$last_share_prompt_round
    if (!is.na(last_shown) && identical(as.integer(last_shown), as.integer(from_round))) return()
    session$userData$last_share_prompt_round <- from_round
    shiny::showModal(shiny::modalDialog(
      title = shiny::tags$div(
        class = "d-flex align-items-center gap-2",
        shiny::tags$i(class = "bi bi-arrow-repeat text-primary", `aria-hidden` = "true"),
        shiny::tags$span("Roster changed - share an updated schedule")
      ),
      shiny::tags$p(
        style = "font-size: 0.95rem; line-height: 1.45;",
        paste0("Your next schedule now starts at Round ", from_round, ". Download or share the updated file so everyone stays in sync.")
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-primary w-100",
        style = "min-height: 2.75rem;",
        onclick = "var el=document.getElementById('download_schedule_html'); if(el) el.click();",
        shiny::tagList(
          shiny::tags$i(class = "bi bi-download me-2", `aria-hidden` = "true"),
          "Download updated schedule (HTML)"
        )
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-outline-primary w-100",
        style = "min-height: 2.75rem;",
        onclick = "if(typeof bbShareScheduleHtml==='function')bbShareScheduleHtml();",
        `aria-label` = "Share schedule HTML to another app",
        shiny::tagList(
          shiny::tags$i(class = "bi bi-share me-2", `aria-hidden` = "true"),
          "Share updated schedule"
        )
      ),
      easyClose = TRUE,
      footer = shiny::modalButton("Not now")
    ))
  })

  # Leave session button removed from UI (host-only workflow). The handler is kept intentionally
  # absent so the session isn't accidentally ended mid-climb from the Session screen.

  # Next round: commit the current round and refresh the upcoming burst.
  shiny::observeEvent(input$next_round_btn, {
    rc <- session$userData$room_code
    if (is.null(rc)) return()
    if (!isTRUE(advance_round_from_burst(rc))) {
      shiny::showNotification("Could not advance (missing session or not enough participants).", type = "warning", duration = 6)
      return()
    }
    ensure_burst(rc, burst_size = BB_BURST_ROUNDS)
  })

  # Category change observer removed (category selection UI removed).
}

shiny::shinyApp(ui, server)
