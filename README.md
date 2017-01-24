# Yummy Goodness
CS:GO's ingame economy system extracted into its own game for [@eevee's GAMES MADE QUICK???](https://itch.io/jam/games-made-quick)

> ...like trying to open a cursed egyptian tomb - [@eevee](https://twitter.com/eevee/status/823806495917604866)

## but why?
Game jams are cool and I want to participate in more of them.

I'm not very good at the economy side of CS:GO so I wanted to get better.

I've been wanting to learn Elm for a while now and I thought this was as good a time as any.

## okay but seriously why did you call it that
It's a reference to [this tutorial thing](https://youtu.be/DpXtRbggpQM?t=4m40s).

## Build Process
For a simple build, just run `elm-make src/Main.elm --output dist/app.js` and open `dist/index.html`.

For the whole shebang:
```sh
npm install -g inliner
elm-make --yes src/Main.elm --output dist/app.js
mkdir out
inliner dist/index.html > out/index.html
```

## FEATURES
- Players have proper inventory (UI: 🙂)
- Players are on teams (UI: 🙂)
- Buy things with the actual CS:GO buy menu (UI: 🙂)
    - Deemphasize submenu if nothing inside is affordable
- Buy things for each player (UI: 😐, make it easier to see that you can select players)
- Make enemies buy things too
- Make players drop weapons (UI: ☹️, make ground not be ugly)
- Simulate rounds of game (UI: 🙂)
    - In each matchup, players shoot at each other as fast as their most primary weapons will allow
    - Players have 50% chance to miss every shot they take
    - This happens anywhere from 3 to 20 times per round but skips if one player is dead
    - Game ends after 15 rounds, so it simulates one half of a full CS:GO match
    - The team with more players alive at the end wins; ties go to CTs
    - You can see who won which round, and you get more money if you're on a losing streak (UI: 🙂)
    - See who killed who with what last round (UI: 🙂 with images and colors blatantly stolen from http://tools.dathost.net/killfeed-generator/)
- There's a tutorial

## TODO
- Simulate rounds of game
    - Plant and defuse the bomb
    - Shots hit head/body/legs
    - Grenades aren't useless
- Appease the gods of line length and dead code
