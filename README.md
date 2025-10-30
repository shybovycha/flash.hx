# flash.hx

This is a [Helix](https://helix-editor.com/) plugin similar to [flash.nvim](https://github.com/folke/flash.nvim) and [leap.nvim](https://github.com/ggandor/leap.nvim).

This plugin uses single-letter jump labels, unlike the default behaviour of `goto_word` (<kbd>g</kbd> <kbd>w</kbd>) which uses two-letter labels.

[output.webm](https://github.com/user-attachments/assets/9cbadc35-9791-4c0a-ae86-abcbb4ffad2f)

## Installation

First you need to obtain a fork of Helix with a [working draft](https://github.com/helix-editor/helix/pull/8675) of a plugin system by cloning and building the corresponding [branch](https://github.com/mattwparas/helix/tree/steel-event-system). Note that you might have to enable a `steel` feature in `helix-term/Cargo.toml` file:

```diff
--- a/helix-term/Cargo.toml
+++ b/helix-term/Cargo.toml
@@ -31,7 +31,7 @@ assets = [
 ]

 [features]
-default = ["git"] # Add steel here for development
+default = ["git", "steel"] # Add steel here for development
 unicode-lines = ["helix-core/unicode-lines", "helix-view/unicode-lines"]
 integration = ["helix-event/integration_test"]
 git = ["helix-vcs/git"]
```

Then you will be able to use the Scheme / [Steel](https://github.com/mattwparas/steel) configuration files in `~/.config/helix/` directory.
You need to copy the `flash.scm` file from this repo to the `~/.config/helix/` directory add the following code to the `init.scm` file:

```scheme
(require "flash.scm")
```

Upon reloading Helix (the one built from the plugin system branch), you will be able to use `:flash` command.
But it is much more handy if you register a keyboard shortcut in the `init.scm`:

```scheme
(keymap (global) (normal (g (/ ":flash"))))
```

Then you can access the functionality using <kbd>g</kbd> <kbd>/</kbd>.

## Configuration

The list of labels could be configured using the existing `jump-label-alphabet` config option under the `[editor]` section in the `~/.config/helix/config.toml` file.

## Known limitations

Currently, there are quite a few limitations in this plugin, most of them due to a work-in-progress nature of the plugin system:

* it only supports forward lookups from the cursor line (but it is possible to alter the implementation to perform lookups on the entire screen or backwards from the cursor line)
* there might be bugs and glitches (the implementation is rather convoluted due to many other limitations of the plugin system and Steel language)

