Toolbox
=======

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

[![Build](https://travis-ci.org/trskop/toolbox-tray-icon.svg)](https://travis-ci.org/trskop/toolbox-tray-icon)


Description
-----------

Tray icon with configurable popup menu.


Screenshots
-----------

![Xfce screenshot without popup menu](screenshot/toolbox-tray-icon-xfce-01.png)
![Xfce screenshot with popup menu](screenshot/toolbox-tray-icon-xfce-02.png)


Configuration
-------------

Configuration file is a simple [JSON][json.org] file that resides in data
directory, see [Cabal User Guide: Installation paths][] for details about what
it means.

Default configuration file is provided and it has following content:

````json
[ { "id": "firefox-profile-manager-no-remote"
  , "name": "Firefox Profile Manager (no remote)"
  , "description": "firefox -ProfileManager -no-remote"
  , "command": "firefox -ProfileManager -no-remote &"
  }
, { "id": "rxvt"
  , "name": "RXVT Unicode"
  , "description": "RXVT Unicode"
  , "command": "rxvt &"
  }
]
````

Most of it is self-explanatory, but few things should be pointed out.

* Property `id` is currently used only by GTK backend and it is used to
  identify specific menu item. For this reason it has to be unique.
* Notice that all commands have `&` at the end. This is due to the fact that we
  use `system` to execute commands and leaving it in a foreground would block
  GUI.



[Cabal User Guide: Installation paths]:
  https://www.haskell.org/cabal/users-guide/installing-packages.html#installation-paths
  "Cabal User Guide: Installation paths"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[json.org]:
  http://json.org/
  "JSON definition and homepage"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
