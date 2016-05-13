## Testing Setup

I use [spacemacs](http://spacemacs.org/) for development in Haskell and I like to use Guard to watch my source and unit test files for changes so that it can automatically run those tests for me, which is nice for TDD/BDD. The following describes how to get Guard working and is no way specific to spacemacs or emacs.

### Guard

Basic steps to get [Guard](https://github.com/guard/guard) working:

Guard requires Ruby version 2.2 or higher. Ironically the easiest way I have found to install this is to [compile from source](https://www.ruby-lang.org/en/documentation/installation/#building-from-source), but there are [other ways](https://www.ruby-lang.org/en/documentation/installation/). I personally would not bother with the extra complexity of using RVM to install Ruby unless you need to use its features. RVM requires your shell to run as a login shell, which is fine but I have run into bugs getting the bashrc file to source correct from a shell in spacemacs.

Now after installing ruby the command `ruby -v` should return something like `ruby 2.3.1`

Guard encourages that it is installed through a Ruby Gem called Bundler. To installed that do: `gem install bundler`.

Bundler lets you specify dependencies in a Gemfile. Enter the command `bundle init` and Bundler will create a cookie cutter Gemfile. However, you can just cut to the chase by creating your own file and entering in something like this:

```ruby
source "https://rubygems.org"

group :development do
  gem 'guard-shell'
end
```

Now guard-shell (info on why I am using this below) is actually a plugin to guard, but I think by specifying that dependency it will automatically bring in guard along with it. If not, then add a `gem 'guard'` line in there also.

Now that you specified that dependency type `bundle` and it should install Guard properly.

Commands to Guard are supposed to be prepended by `bundle exec ` since it is managing those dependencies for you.

You an create cookie cutter guard file by doing: `bundle exec guard init`

Or just place the following in a file called 'Guardfile':

```ruby
# the regex in the watch method matches on file names that end with .hs
# except for file names that contain 'flycheck' in their name since those
# are generated in the background by emacs.
guard :shell do
  watch(%r{^(?!.*flycheck).+\.1?hs$}) do
    system('stack runhaskell *Spec.hs')
  end
end
```

Now run guard by typing: `bundle exec guard`

### Guard-Shell

Guard has a bunch of plugins that extend it's functionality. I am using [guard-shell](https://github.com/guard/guard-shell) instead of [guard-haskell](https://github.com/supki/guard-haskell) because the latter requires a proper project structure setup with a cabal file and all of that which is overkill for what I am trying to do here with basic "learning the language" type of exercises.
