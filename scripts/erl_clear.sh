ps aux | grep -e heart -e epmd | grep -v grep | tr -s ' ' | cut -d ' ' -f 2 | xargs kill -9
