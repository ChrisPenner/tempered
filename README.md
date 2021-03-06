Tempered
======

[![Hackage](https://img.shields.io/badge/hackage-latest-green.svg)](https://hackage.haskell.org/package/tempered)

A dead-simple templating utility for simple shell interpolation.
Use at your own risk and only on trusted templates.

```bash
$ tempered --help
Tempered - Templating engine based on shell interpolation

Usage: tempered [templates]
  Interpolate templates to stdout

Available options:
  templates                The templates to interpolate
  -h,--help                Show this help text
```

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

Then we can render the template with 

```
tempered ./post.md

# OR

cat ./post.md | tempered
```

to get:

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

Shell not powerful enough for you? No problem! Use all your favourite languages and
tools; See the FAQ or examples to see how to integrate with scripts (spoilers, just call them like you do in bash!)


[**Examples Here**](https://github.com/ChrisPenner/tempered/tree/master/examples)

Installation
============

Mac and Linux binaries are provided [HERE](https://github.com/ChrisPenner/tempered/releases/latest);

### Homebrew On Mac

```
brew update
brew install ChrisPenner/tools/tempered
```

### Stack
If you're familiar with Haskell Stack:

```
stack update && stack install tempered
```

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

### Other Scripts/Tools?

Duh! Interpolation works like a shell, just call the scripts or binaries you want!
Here we'll use a simple python script to spice things up!

```python
# favourites.py
import sys

print(" and ".join(sys.argv[1:]) + ",")
print("These are a few of my favourite things")
```

```
# My Favourite Things

{{ python favourites.py Gumdrops Roses Whiskers Kittens }}

When the dog bites
When the bee stings
When I'm feeling sad
I simply remember my favourite things
And then I don't feel so bad
```

Output:

```
# My Favourite Things

Gumdrops and Roses and Whiskers and Kittens,
These are a few of my favourite things

When the dog bites
When the bee stings
When I'm feeling sad
I simply remember my favourite things
And then I don't feel so bad
```

### \_\_: command not found?

Chances are you're forgetting to echo an env-var;
`{{ $TITLE }}` will try to run the contents of `$TITLE` as a command, you want
`{{ echo "$TITLE" }}`.

### Isn't this whole thing a security risk?

Probably.
