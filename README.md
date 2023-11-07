# erlang-flat-figure

## Import c++ project to Erlang

Simple application on Erlang for parcing .txt file:

```
add figure params color date    -- adding figure to program
rem figure params               -- remove figure with specific params
rem figure color                -- remove all figure have a color
rem *                           -- just remove all figure
print                           -- print cur figures
```

Example of input file located in root dir.

> Careful! Very dirty code!

## TODO:

- [x] remove `\n` symbol in line if exist
- [x] add specific shapes (circle, rectangle, triangle)
- [x] remove trash in outputing
- [ ] solve the container problem (may rethink the data storage structure)
- [ ] add function `rem *figure* *color*`
- [ ] add list allowed colors
- [ ] add regex to input strings
- [ ] structure project using best practices
- [ ] use a linter for erlang