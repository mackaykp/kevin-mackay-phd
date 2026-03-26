# Belay Buddy Website Demo

Static offline demo for showcasing the app on a website (no Shiny server runtime).

## Files

- `index.html` - demo page
- `demo.css` - demo styling
- `demo.js` - interactive behavior (host join, add/remove, rounds, recents)

`demo.css` imports `./custom.css` so the demo is self-contained and portable.

## What this demo keeps

- Host join flow (including recent host names)
- Add/remove participants flow (including recent participant names)
- Participant chips, roster change history, "Next round" behavior
- 5 upcoming rounds shown as now/upcoming cards
- Canned round templates for 2-10 active participants

## What this demo does not do

- Real pairing optimization/calculation from the R backend
- Multi-user shared live session state

For active participant counts outside 2-10, the demo uses a simple fallback pairing pattern.

## How to use

Open `index.html` in a browser, or upload the entire `website_demo` folder to static hosting.

The folder already includes its own `custom.css` copy, so you can move `website_demo` as-is.

