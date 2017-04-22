#!/Users/chris/.local/bin/plated-exe
{{cat | tr 'a-z' 'A-Z' <<EOF
testing
}}

{{ cat ./lorem.md }}

Other text

Title: {{ echo $title }}
Date: {{ echo $date }}

go!
