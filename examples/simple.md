{{ # inside "post.md" }}
# {{ echo $TITLE | tr 'a-z' 'A-Z' }}
by {{ echo $USER }}
Published on {{ echo $DATE }}

Here's my blog post!

{{ for i in 99 98 97 ; do
    cat <<EOF
$i bottles of beer on the wall
EOF
done }}

---
{{ cat ./footer.md }}
