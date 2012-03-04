# StrifeBarge
###### Attack of the strategic thesaurus

This is an HTTP, multiplayer guessing game

### Notes

- Semi-playable, graphic version is now available (only supports one game per server at the moment, so just stick to playing with your local friends)
- Makes heavy use of HTML5 and CSS3 constructs; recent browsers advised

### Usage

You can *almost* `ql:quickload` it. It uses a package called `cl-css` to eliminate a bunch of CSS3 boilerplate, so you'll need to install that via ASDF or [github](https://github.com/Inaimathi/cl-css).

    (require 'asdf) 
    (require 'asdf-install) 
    (asdf-install:install 'cl-css)` 

1. Clone this repository
2. Start up your lisp
3. `(ql:quickload :strifebarge)`
4. Browse to `http://[your-machine-ip]:5050` and have your opponent do the same
5. One of you start a **New Game**. The other should then **Join** it.
6. Play **StrifeBarge**

### License Info

This program is released under the GNU AGPL (License text can be found in the LICENSE.md file, or at <http://www.gnu.org/licenses/>)

A minified copy of jQuery is included for ease of installation; jQuery is dual-licensed under the [GPL and Expat licenses](http://jquery.org/license/). Readable source can be found at [their project page](http://jquery.com/).

Media is included from OpenGameArt.org. This includes

- [crosshair icons](http://opengameart.org/content/crosshairs-and-reticles) (public domain by hackcraft.de)
- [explosion](http://opengameart.org/content/explosion) (public domain by Cuzco)
- [spaceship images](http://opengameart.org/content/spaceships-top-down) (dual CC-BY-SA/GPL by Skorpio)
- [star-field background](http://opengameart.org/content/galaxy-skybox) (dual CC-BY/CC-BY-SA by hackcraft.de)

more to come, I'm sure.
