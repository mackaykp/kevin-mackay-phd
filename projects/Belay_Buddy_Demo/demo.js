(function () {
  const HOST_KEY = "belayBuddy_recent_host_names_v1";
  const PARTICIPANT_KEY = "belayBuddy_recent_names_v1";
  const MAX_RECENT = 20;

  const state = {
    created: false,
    joined: false,
    hostId: null,
    round: 1,
    participants: [],
    rosterEvents: []
  };

  // Canned round templates by participant count.
  // Pair = [a,b], Trio = [a,b,c] (index positions in active participants list).
  const CANNED = {
    2: [
      [[0, 1]]
    ],
    3: [
      [[0, 1, 2]],
      [[1, 2, 0]],
      [[2, 0, 1]]
    ],
    4: [
      [[0, 1], [2, 3]],
      [[0, 2], [1, 3]],
      [[0, 3], [1, 2]]
    ],
    5: [
      [[0, 1, 2], [3, 4]],
      [[1, 2, 3], [4, 0]],
      [[2, 3, 4], [0, 1]],
      [[3, 4, 0], [1, 2]],
      [[4, 0, 1], [2, 3]]
    ],
    6: [
      [[0, 1], [2, 3], [4, 5]],
      [[0, 2], [1, 4], [3, 5]],
      [[0, 3], [1, 5], [2, 4]],
      [[0, 4], [1, 3], [2, 5]],
      [[0, 5], [1, 2], [3, 4]]
    ],
    7: [
      [[0, 1, 2], [3, 4], [5, 6]],
      [[1, 2, 3], [4, 5], [6, 0]],
      [[2, 3, 4], [5, 6], [0, 1]],
      [[3, 4, 5], [6, 0], [1, 2]],
      [[4, 5, 6], [0, 1], [2, 3]]
    ],
    8: [
      [[0, 1], [2, 3], [4, 5], [6, 7]],
      [[0, 2], [1, 3], [4, 6], [5, 7]],
      [[0, 3], [1, 2], [4, 7], [5, 6]],
      [[0, 4], [1, 5], [2, 6], [3, 7]],
      [[0, 5], [1, 6], [2, 7], [3, 4]]
    ],
    9: [
      [[0, 1, 2], [3, 4], [5, 6], [7, 8]],
      [[1, 2, 3], [4, 5], [6, 7], [8, 0]],
      [[2, 3, 4], [5, 6], [7, 8], [0, 1]],
      [[3, 4, 5], [6, 7], [8, 0], [1, 2]],
      [[4, 5, 6], [7, 8], [0, 1], [2, 3]]
    ],
    10: [
      [[0, 1], [2, 3], [4, 5], [6, 7], [8, 9]],
      [[0, 2], [1, 3], [4, 6], [5, 7], [8, 9]],
      [[0, 3], [1, 4], [2, 5], [6, 8], [7, 9]],
      [[0, 4], [1, 5], [2, 6], [3, 7], [8, 9]],
      [[0, 5], [1, 6], [2, 7], [3, 8], [4, 9]]
    ]
  };

  let pidSeq = 1;

  const els = {
    startView: document.getElementById("start-view"),
    sessionView: document.getElementById("session-view"),
    hostJoinCard: document.getElementById("host-join-card"),
    createBtn: document.getElementById("create-btn"),
    hostName: document.getElementById("host-name"),
    hostJoinBtn: document.getElementById("host-join-btn"),
    hostRecentChips: document.getElementById("host-recent-chips"),
    addPersonName: document.getElementById("add-person-name"),
    addPersonBtn: document.getElementById("add-person-btn"),
    participantRecentChips: document.getElementById("participant-recent-chips"),
    participantsTitle: document.getElementById("participants-title"),
    participantsList: document.getElementById("participants-list"),
    rosterCard: document.getElementById("roster-card"),
    rosterList: document.getElementById("roster-list"),
    nextRoundBtn: document.getElementById("next-round-btn"),
    roundCards: document.getElementById("round-cards")
  };

  function readRecent(key) {
    try {
      const x = JSON.parse(localStorage.getItem(key) || "[]");
      return Array.isArray(x) ? x : [];
    } catch (e) {
      return [];
    }
  }

  function pushRecent(key, name) {
    const n = String(name || "").trim();
    if (!n) return;
    const arr = readRecent(key).filter((x) => x !== n);
    arr.unshift(n);
    localStorage.setItem(key, JSON.stringify(arr.slice(0, MAX_RECENT)));
  }

  function activeParticipants() {
    return state.participants.filter((p) => p.active !== false);
  }

  function addParticipant(name, isHost) {
    const n = String(name || "").trim();
    if (!n) return null;
    if (activeParticipants().some((p) => p.name.toLowerCase() === n.toLowerCase())) return null;
    const p = { id: "p" + pidSeq++, name: n, active: true };
    state.participants.push(p);
    if (isHost) state.hostId = p.id;
    return p;
  }

  function removeParticipant(id) {
    const p = state.participants.find((x) => x.id === id);
    if (!p) return;
    p.active = false;
    state.rosterEvents.push({
      type: "remove",
      name: p.name,
      effectiveRound: state.round + 1,
      ts: Date.now()
    });
  }

  function recordAdd(name) {
    state.rosterEvents.push({
      type: "add",
      name,
      effectiveRound: state.round + 1,
      ts: Date.now()
    });
  }

  function fallbackGroups(n, roundNumber) {
    const idx = [...Array(n).keys()];
    const offset = (roundNumber - 1) % n;
    const rot = idx.slice(offset).concat(idx.slice(0, offset));
    const groups = [];
    if (n % 2 === 1 && n >= 3) groups.push([rot[0], rot[1], rot[2]]);
    const start = n % 2 === 1 ? 3 : 0;
    for (let i = start; i + 1 < rot.length; i += 2) groups.push([rot[i], rot[i + 1]]);
    return groups;
  }

  function pairingsForRound(names, roundNumber) {
    const n = names.length;
    if (n < 2) return [];
    const template = CANNED[n];
    const groups = template
      ? template[(roundNumber - 1) % template.length]
      : fallbackGroups(n, roundNumber);
    const rows = [];
    let pairNum = 1;
    groups.forEach((g) => {
      if (g.length === 3) {
        rows.push({
          label: "Trio",
          order: [names[g[0]], names[g[1]], names[g[2]]]
        });
      } else if (g.length === 2) {
        rows.push({
          label: `Pair ${pairNum++}`,
          a: names[g[0]],
          b: names[g[1]]
        });
      }
    });
    return rows;
  }

  function paintRecentChips(container, key, onPick) {
    container.textContent = "";
    readRecent(key).forEach((name) => {
      const b = document.createElement("button");
      b.type = "button";
      b.className = "btn btn-sm bb-recent-chip";
      b.textContent = name;
      b.addEventListener("click", () => onPick(name));
      container.appendChild(b);
    });
  }

  function renderParticipants() {
    const active = activeParticipants();
    els.participantsTitle.textContent = `Participants (${active.length})`;
    els.participantsList.textContent = "";
    if (!active.length) {
      const p = document.createElement("p");
      p.className = "muted-note mb-0";
      p.textContent = "No active participants yet.";
      els.participantsList.appendChild(p);
      return;
    }
    const list = document.createElement("div");
    list.className = "participant-simple-list";
    active.forEach((p) => {
      const row = document.createElement("div");
      row.className = "participant-chip-row";
      const chip = document.createElement("div");
      chip.className = "participant-chip participant-chip-other";
      if (p.id === state.hostId) chip.className = "participant-chip you";
      chip.textContent = p.id === state.hostId ? `${p.name} (host)` : p.name;
      row.appendChild(chip);

      if (p.id !== state.hostId) {
        const rm = document.createElement("button");
        rm.type = "button";
        rm.className = "btn btn-link bb-participant-remove";
        rm.innerHTML = '<i class="bi bi-x-lg"></i>';
        rm.setAttribute("aria-label", `Remove ${p.name}`);
        rm.addEventListener("click", () => {
          removeParticipant(p.id);
          renderAll();
        });
        row.appendChild(rm);
      }
      list.appendChild(row);
    });
    els.participantsList.appendChild(list);
  }

  function renderRosterChanges() {
    const events = state.rosterEvents.slice().sort((a, b) => b.effectiveRound - a.effectiveRound || b.ts - a.ts);
    els.rosterList.textContent = "";
    if (!events.length) {
      els.rosterCard.classList.add("d-none");
      return;
    }
    els.rosterCard.classList.remove("d-none");

    const rounds = [...new Set(events.map((e) => e.effectiveRound))];
    rounds.forEach((r) => {
      const group = document.createElement("div");
      group.className = "roster-group";
      const lbl = document.createElement("span");
      lbl.className = "roster-round";
      lbl.textContent = `Eff ${r}`;
      group.appendChild(lbl);

      const chips = document.createElement("div");
      chips.className = "roster-chips";
      events.filter((e) => e.effectiveRound === r).forEach((e) => {
        const chip = document.createElement("div");
        chip.className = "participant-chip participant-chip-other " + (e.type === "add" ? "participant-chip-added" : "participant-chip-removed");
        chip.textContent = e.name;
        chips.appendChild(chip);
      });
      group.appendChild(chips);
      els.rosterList.appendChild(group);
    });
  }

  function makePairingRows(rows) {
    return rows.map((r, i) => {
      const outer = document.createElement("div");
      if (r.label === "Trio") {
        outer.className = "pairing-row pairing-row-trio";
        outer.innerHTML = `
          <div class="pairing-label">Trio</div>
          <div class="pairing-trio">
            <div class="pairing-trio-slot">
              <i class="bi bi-arrow-up-circle-fill pairing-icon pairing-icon-climb"></i><span class="pairing-name">${r.c1}</span>
              <i class="bi bi-link-45deg pairing-icon pairing-icon-belay"></i><span class="pairing-name">${r.b1}</span>
              <i class="bi bi-pause-circle pairing-icon pairing-icon-rest"></i><span class="pairing-name">${r.r1}</span>
            </div>
            <div class="pairing-trio-slot">
              <i class="bi bi-arrow-up-circle-fill pairing-icon pairing-icon-climb"></i><span class="pairing-name">${r.c2}</span>
              <i class="bi bi-link-45deg pairing-icon pairing-icon-belay"></i><span class="pairing-name">${r.b2}</span>
              <i class="bi bi-pause-circle pairing-icon pairing-icon-rest"></i><span class="pairing-name">${r.r2}</span>
            </div>
            <div class="pairing-trio-slot">
              <i class="bi bi-arrow-up-circle-fill pairing-icon pairing-icon-climb"></i><span class="pairing-name">${r.c3}</span>
              <i class="bi bi-link-45deg pairing-icon pairing-icon-belay"></i><span class="pairing-name">${r.b3}</span>
              <i class="bi bi-pause-circle pairing-icon pairing-icon-rest"></i><span class="pairing-name">${r.r3}</span>
            </div>
          </div>
        `;
      } else {
        outer.className = "pairing-row pairing-row-pair";
        outer.innerHTML = `
          <div class="pairing-label">${r.pairLabel || `Pair ${i + 1}`}</div>
          <div class="pairing-pair-line">
            <div class="pairing-visual">
              <div class="pairing-slot pairing-slot-climb">
                <span class="pairing-name">${r.a}</span>
                <i class="bi bi-arrow-up-circle-fill pairing-icon pairing-icon-climb" title="Climbs first"></i>
              </div>
              <span class="pairing-sep">·</span>
              <div class="pairing-slot pairing-slot-belay">
                <span class="pairing-name">${r.b}</span>
                <i class="bi bi-link-45deg pairing-icon pairing-icon-belay" title="Belays first"></i>
              </div>
            </div>
            <div class="pairing-swap">
              <i class="bi bi-arrow-left-right pairing-icon-swap" aria-hidden="true"></i>
              <span class="pairing-swap-text">then swap</span>
            </div>
          </div>
        `;
      }
      return outer;
    });
  }

  function renderRounds() {
    const active = activeParticipants();
    els.roundCards.textContent = "";
    if (active.length < 2) {
      const c = document.createElement("div");
      c.className = "card bb-card mb-2";
      c.innerHTML = '<div class="card-body text-muted small">Add at least 2 participants to show rounds.</div>';
      els.roundCards.appendChild(c);
      return;
    }
    const names = active.map((p) => p.name);
    for (let k = 0; k < 5; k++) {
      const absRound = state.round + k;
      const ac = ((absRound - 1) % 5) + 1;
      const subtitle = k === 0 ? "now" : "upcoming";
      const card = document.createElement("div");
      card.className = `card bb-card bb-round-card bb-accent-${ac} mb-1`;
      card.innerHTML = `
        <div class="card-header bb-round-header">
          <span class="bb-round-badge bb-round-badge-${ac}">${absRound}</span>
          <span class="bb-round-header-text">
            <span class="bb-round-title">Round ${absRound}</span>
            <span class="bb-round-sub">${subtitle}</span>
          </span>
        </div>
      `;
      const body = document.createElement("div");
      body.className = "card-body bb-pairings-body";
      const pairingRows = pairingsForRound(names, absRound).map((r) => {
        if (r.label === "Trio") {
          const ord = r.order;
          return {
            label: "Trio",
            c1: ord[0], b1: ord[1], r1: ord[2],
            c2: ord[1], b2: ord[2], r2: ord[0],
            c3: ord[2], b3: ord[0], r3: ord[1]
          };
        }
        return { label: "Pair", pairLabel: r.label || "Pair", a: r.a, b: r.b };
      });
      makePairingRows(pairingRows).forEach((row) => body.appendChild(row));
      card.appendChild(body);
      els.roundCards.appendChild(card);
    }
  }

  function renderAll() {
    paintRecentChips(els.hostRecentChips, HOST_KEY, (name) => {
      els.hostName.value = name;
      joinAsHost();
    });
    paintRecentChips(els.participantRecentChips, PARTICIPANT_KEY, (name) => {
      els.addPersonName.value = name;
      addPersonFromInput();
    });
    renderParticipants();
    renderRosterChanges();
    renderRounds();
  }

  function joinAsHost() {
    const name = els.hostName.value.trim();
    if (!name) return;
    if (!addParticipant(name, true)) {
      alert(`Someone named "${name}" is already in this room.`);
      return;
    }
    pushRecent(HOST_KEY, name);
    state.joined = true;
    els.startView.classList.add("d-none");
    els.sessionView.classList.remove("d-none");
    renderAll();
  }

  function addPersonFromInput() {
    const name = els.addPersonName.value.trim();
    if (!name) return;
    if (!addParticipant(name, false)) {
      alert(`"${name}" is already in this room.`);
      return;
    }
    recordAdd(name);
    pushRecent(PARTICIPANT_KEY, name);
    els.addPersonName.value = "";
    renderAll();
  }

  els.createBtn.addEventListener("click", () => {
    state.created = true;
    els.hostJoinCard.classList.remove("d-none");
    renderAll();
  });
  els.hostJoinBtn.addEventListener("click", joinAsHost);
  els.addPersonBtn.addEventListener("click", addPersonFromInput);
  els.nextRoundBtn.addEventListener("click", () => {
    state.round += 1;
    renderRounds();
  });
  els.hostName.addEventListener("keydown", (e) => {
    if (e.key === "Enter") joinAsHost();
  });
  els.addPersonName.addEventListener("keydown", (e) => {
    if (e.key === "Enter") addPersonFromInput();
  });

  renderAll();
})();
