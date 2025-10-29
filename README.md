# flash.hx

This is a [Helix](https://helix-editor.com/) plugin similar to [flash.nvim](https://github.com/folke/flash.nvim) and [leap.nvim](https://github.com/ggandor/leap.nvim).

[demo.webm](/shybovycha/flash.hx/refs/heads/master/demo.webm)

## Installation

First you need to obtain a fork of Helix with a [working draft](https://github.com/helix-editor/helix/pull/8675) of a plugin system by cloning and building the corresponding [branch](https://github.com/mattwparas/helix/tree/steel-event-system).

Then you will be able to use the Scheme / [Steel](https://github.com/mattwparas/steel) configuration files in `~/.config/helix/` directory.
You need to copy the `flash.scm` file from this repo to the `~/.config/helix/` directory add the following code to the `init.scm` file:

```scheme
(require "flash.scm")
```

Upon reloading Helix (the one built from the plugin system branch), you will be able to use `:flash` command.

## Known limitations

Currently, there are quite a few limitations in this plugin, most of them due to a work-in-progress nature of the plugin system:

* plugin only works in normal mode (visual mode is not supported)
* it only supports forward lookups from the cursor line (but it is possible to alter the implementation to perform lookups on the entire screen or backwards from the cursor line)
* no configuration is provided (since there is no API in the plugin system to query the config file)
* binding to a key shortcut (like <kbd>g</kbd> <kbd>/</kbd>) does not seem to work
* there might be bugs and glitches (the implementation is rather convoluted due to many other limitations of the plugin system and Steel language)

