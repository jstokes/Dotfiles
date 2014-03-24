for file in ~/env/.*
  do if [[ $file != *.swp*  ]] then
    source "$file"
  fi
done
