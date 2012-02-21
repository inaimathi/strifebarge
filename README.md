# StrifeBarge
###### Attack of the strategic thesaurus

This is an HTTP, multiplayer implementation of the classic [battleship pen & paper](http://en.wikipedia.org/wiki/Battleship_(game)) game.

### Notes

- Do not attempt to play it yet; it only supports one game at a time and barfs at things which have no business being barfed at.
- Things are extra OO for later expansion

### Usage

#### Don't.

Ok, if you **must**. 

1. Clone this repository
2. Start up your lisp
3. `(ql:quickload :strifebarge)`
4. Browse to `http://localhost:5050/new-game`
5. Have your opponent browse to `http://[your-machine-ip]:5050/join-game`
6. Play **StrifeBarge**

If you get an error, the most probable reason is that you went off-turn. Just hit the *Back* button and wait for your opponent to go. If they're too much of a dick to continue, tell them to fuck off and do a little victory dance to commemorate their concession.
