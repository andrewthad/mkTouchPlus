# mkTouchPlus

Combines `mkdir -p` and `touch`, automatically formats names and can make multiple files and folder at once. A huge time-saver that I made so that I could use it everyday, especially to auto-hyphenate my filepaths.

![Screenshot of mkTouchPlus in the terminal](https://github.com/benjamin-glitsos/mkTouchPlus/blob/master/mkTouchPlus-screenshot.jpg)

## Requirements

### Using binaries

* Linux or Mac

### Compiling it yourself

* Linux or Mac
* [GHC](https://www.haskell.org/ghc/) 10 or greater. Most people recommend installing [Stack](https://docs.haskellstack.org/en/stable/README/) which includes GHC. This script doesn’t depend on any Haskell packages outside of the base, so that makes things a lot easier.

## Installation

### Using binaries

1. Put the files in the `binary` folder of this repository into your system’s `bin` folder.
2. Make a function in your `.bashrc` or `.zshrc` that calls your compiled script. Like so:

```
m() { echo "$*" | /home/bengyup/bin/mkTouchPlus/mkTouchPlus }
```

You can use that shell function without even quoting its input like this: `m make / this.txt`. However, keep in mind that certain characters need to be escaped in Bash. To input these, you can use escape slashes: `m make \& this`, or surrounding quotes: `m ";;;;;make %$ this"`.

Also, you can create shell functions that pass different settings to mkTouchPlus. Like this:

```
ms() { echo ";snakeSep;;;;$*" | /home/bengyup/bin/mkTouchPlus/mkTouchPlus }
```

### Compiling it yourself

1. Install GHC and compile this script using `ghc -o mkTouchPlus mkTouchPlus.hs`
2. Follow the steps in the above ‘Using binaries’ section

## Usage

You can use `mkTouchPlus` whenever you would otherwise use the `touch` or `mkdir -p` commands. By default, `mkTouchPlus` will automatically determine whether you want to create a file or directory.

mkTouchPlus will format your file and directory and file-extension names in a consistent and automatic fashion. To configure this formatting, or to determine what output type (file, folder or echo) will be created, you can pass arguments to the command. These settings have the format of exactly five semicolon-separated values preceding the usual input. E.g. `m ";;u;;;this will be uppercase"`

This script can also create multiple files and folders at a time if you separate the names of these outputs with commas. E.g. `make this, and this.txt, and this`

To see all the options that can be set, and also for some example inputs and a key to the color-coding, run `mkTouchPlus -h`. You will then see a colored version of:

```

  
  mkTouchPlus v1.0

  Create one or more files and directory paths, with automatic name formatting.
  
  USAGE:

  mkTouchPlus [ioOperation],[separator],[characterCase],[extensionFormat],[sanitisation],[name]
    or
  mkTouchPlus [name]
    or
  mkTouchPlus [--help / --version]

    Where:
      ioOperation     : fileCreate / dirCreate / smartCreate / echo
      separator       : hyphenSep / snakeSep / dotSep / whitespaceSep
      characterCase   : lowerCase / upperCase / titleCase / camelCase / id
      extensionFormat : extSep / hyphenSep / snakeSep / dotSep / whitespaceSep
      sanitisation    : unix / windows / mac / sensible / conservative
      name            : One or more names for files or directories that will be
                          outputted. Comma-separated.

  EXAMPLES:

  create file.txt
  create folder
  create/a/path/
  create/a/path.txt
  this is  /automatic formatting . txt
  "ForMAtting / @ % (/consistency enforced ) ~ . & STYLES"
  multiple.txt,folders,and,files.txt
  .dotFile
  dotted.path/a/b.txt
  f,w,u,s,u,choice of options
  fileCreate,w,u,snakeCase,u,options can be written in full-form
  ,,,,,default options
  also default options
  ,s,,,,snake case with other options as default
  ,s,,,w,options plus/,multiple/,files.txt,or folders/
  ../parent directory/file.txt
  walking/../the/../../file system/
  /start at home directory
  
  OUTPUT:

  The output of this command is color coded. e.g.

  a/b/c/../d/e/file.txt
  
  The color code is:

  blue  for created files
  green for created directories
  white for non-creation events
  red   for errors and skipped creations
  
  For more help, open the readme in your browser:

  https://github.com/benjamin-glitsos/mkTouchPlus/blob/master/README.md

```
