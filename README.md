# vrci

clone
`stack build`
`stack install` 

# usage 

Uses find to pipe in files into it / exclude the directories.  Takes in java files (to find the defined rcis) scans scala files for the usage

```
find . -type d \( -name "web" -o -name ".hg" -o -name ".idea" -o -name ".hgcheck" -o -name "target" \) -prune -o -type f \( -name "*.java" -o -name "*.scala" \)  -print | vrci-exe +RTS -N1
```

# todo 

- need to filter out hasRCI files 
  - need to search either java / scala for the rci _usages_
 
