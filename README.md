# vrci

Uses find to pipe in files to it: 

```
find . -type d \( -name "web" -o -name ".hg" -o -name ".idea" -o -name ".hgcheck" -o -name "target" \) -prune -o -type f \( -name "*.java" -o -name "*.scala" \)  -print | vrci-exe +RTS -N1
```
