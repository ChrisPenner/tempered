Tempered
======

[![Hackage](https://img.shields.io/badge/hackage-latest-green.svg)](https://hackage.haskell.org/package/tempered)

A dead-simple templating utility for simple shell interpolation.
Use at your own risk and only on trusted templates.

Here's a simple use-case:

```md
{{ # inside "post.md" }}
# My Blog Post

by {{ echo $AUTHOR }}
Published on {{ date +'%B %d, %Y' }}

Here's my blog post!

---
{{ cat ./footer.md | tr 'a-z' 'A-Z' }}
```

```md
{{ # inside "footer.md" }}
Copyright 2017 Chris Penner
Check me out on twitter @chrislpenner!
See you next time!
```

Then we can render the template with `tempered ./post.md` to get:

```
# My Blog Post

by Chris Penner
Published on April 22, 2017

Here's my blog post!

---
COPYRIGHT 2017 CHRIS PENNER
CHECK ME OUT ON TWITTER @CHRISLPENNER!
SEE YOU NEXT TIME!
```

If you want you can add a shebang to the top of your template and just run it
as an executable, tempered will strip the shebang for you automagically:

> test.txt
```
#!/path/to/tempered
interpolate {{ echo $THIS }}
```

```bash
$ chmod +x test.txt
$ export THIS="that"
$ ./test.txt
interpolate that
```

[**Examples Here**](https://github.com/ChrisPenner/tempered/tree/master/examples)

Installation
============

`stack install tempered`

FAQ
=====

There's really not much to it; parses the file and runs anything
inside `{{  }}` as a shell expression and pipes stdout into its spot.
If you're clever you can do pretty much anything you want with this.

### Variables?

Sure; It's bash.

```bash
Hello, my name is {{echo $USER}}
```

You can set up environment overrides in `env.yaml`, tempered looks up through
the file-system to find an `env.yaml` from the cwd NOT the template location.

Here's an example env.yaml; we can do simple strings or commands here; just make
sure to quote any entries that start with `{{` or the YAML parser gets mad.

> env.yaml
```yaml
PROJECT: Tempered
DATE: "{{ date +'%B %d, %Y' }}"
```

Then you can use them just like normal variables.

### For Loops? 

It's bash; go for it:
```bash
{{ for i in 99 98 97 ; do
    cat <<EOF
$i bottles of beer on the wall
EOF
done }}
```

> output:
```
99 bottles of beer on the wall
98 bottles of beer on the wall
97 bottles of beer on the wall
```

### \_\_: command not found?

Chances are you're forgetting to echo an env-var;
`{{ $TITLE }}` will try to run the contents of `$TITLE` as a command, you want
`{{ echo "$TITLE" }}`.

### Isn't this whole thing a security risk?

Probably.
