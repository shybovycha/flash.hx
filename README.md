# flash.hx

This is a [Helix](https://helix-editor.com/) plugin similar to [flash.nvim](https://github.com/folke/flash.nvim) and [leap.nvim](https://github.com/ggandor/leap.nvim).

This plugin uses single-letter jump labels, unlike the default behaviour of `goto_word` (<kbd>g</kbd> <kbd>w</kbd>) which uses two-letter labels.

[output.webm](https://github.com/user-attachments/assets/65285c2e-0862-4fdc-951e-7f99f6c8d30b)

## Installation

### Prerequisites
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

### Installing this plugin

You need to copy the `flash.scm` file from this repo to the `~/.config/helix/` directory add the following code to the `init.scm` file:

```scheme
(require "flash.scm")
```

Upon reloading Helix (the one built from the plugin system branch), you will be able to use `:flash`, `:flash-forward` and `:flash-backward` commands.

* `:flash` - performs search on the entire screen
* `:flash-forward` - performs search starting from the current cursor (or just the first one if there are multiple) downwards
* `:flash-backward` - performs search from the top of the screen to the current cursor (or the first one if there are multiple)

But it is much more handy if you register keyboard shortcuts in the `init.scm`:

```scheme
(keymap
  (global)
  (normal
    (g
      (/ ":flash-forward")
      (? ":flash-backward")))
  (select
    (g
      (/ ":flash-forward")
      (? ":flash-backward"))))
```

Then you can access the functionality using <kbd>g</kbd> <kbd>/</kbd> and <kbd>g</kbd> <kbd>?</kbd>.

My personal preference is to set just one keybinding to search and jump on the entire screen with <kbd>g</kbd> <kbd>/</kbd>:

```scheme
(keymap
  (global)
  (normal
    (g
      (/ ":flash"))))
```

## Configuration

The list of labels could be configured using the existing `jump-label-alphabet` config option under the `[editor]` section in the `~/.config/helix/config.toml` file.

## Known limitations

Currently, there are quite a few limitations in this plugin, most of them due to a work-in-progress nature of the plugin system:

* there might be bugs and glitches (the implementation is rather convoluted due to many other limitations of the plugin system, the Steel language and my skill with both)

