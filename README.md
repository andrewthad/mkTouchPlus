# mkTouch+

Combines mkDir (-p) and touch, automatically formats word separators and case and can make multiple files and folder at once. A huge time-saver that I made so I could use it everyday, especially to auto-hyphenate my filepaths.

## Installation


## Usage

`mkTouchPlus -h` produces

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
