# mkTouch+

Combines `mkdir -p` and `touch`, automatically formats word separators and case and can make multiple files and folder at once. A huge time-saver that I made so I could use it everyday, especially to auto-hyphenate my filepaths.

## Requirements

* [GHC](https://www.haskell.org/ghc/) 10 or greater
* (Only tested on Linux)

## Installation

1. Install GHC and compile this script using `ghc -o mkTouchPlus mkTouchPlus.hs`
2. Place the three files of the compiled script into your `bin/` folder
3. Make a function in your `.bashrc` or `.zshrc` that calls your compiled script. Like so:

```
m() { echo "$*" | /home/bengyup/bin/mkTouchPlus/mkTouchPlus }
```

4. You can use that shell function without even quoting its input like this: `m make / this.txt`. However, keep in mind that certain characters need to be escaped in Bash. To input these, you can use escape slashes: `m make \& this`, or surrounding quotes: `m ";;;;;make this"`.

## Usage

You can use `mkTouchPlus` whenever you would otherwise use the `touch` or `mkdir -p` commands. By default, `mkTouchPlus` will automatically determine whether you want to create a file or directory.

mkTouchPlus will format your file and directory and file-extension names in a consistent and automatic fashion. To configure this formatting, or to determine what output type (file, folder or echo) will be created, you can pass arguments to the command. These settings have the format of exactly five semicolon-separated values preceding the usual input. E.g.

`m ";;u;;;this will be uppercase"`

To see all the options that can be set, and also for some example inputs and a key to the color-coding, run `mkTouchPlus -h`. You will then see a colored version of:

``````
  mkTouch+ v1.0

  Create one or more files and directory paths, with automatic name formatting.
  
  Usage:

  mkTouchPlus [ioOperation],[separator],[characterCase],[extensionFormat],[sanitisation],[name]
    or
  mkTouchPlus [name]
    or
  mkTouchPlus [-h / -v]

    where:
      ioOperation     : fileCreate / dirCreate / smartCreate / putStrLn / smartCreate
      separator       : hyphenSep / snakeSep / dotSep / whitespaceSep
      characterCase   : lowerCase / upperCase / titleCase / camelCase / id
      extensionFormat : extSep / hyphenSep / snakeSep / dotSep / whitespaceSep
      sanitisation    : unix / windows / mac / sensible / conservative
      name            : One or more names for files or directories that will be
                          outputted. Continues the comma-separated list of arguments.

  
  For more help, open the readme in your browser:

  https://www.com

``````
