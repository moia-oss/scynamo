# Maintainer Manual

## Release a new version

Every new commit in master is built and released as a -SNAPSHOT. Once you tested that everything works, do the following to create an actual release.

Create an empty commit using git commit --allow-empty and specify which version you are releasing.

Tag (annotated tag with description) the new commit with a v prefix, e.g. v1.0.0. The sbt-git plugin automatically sets the version to the tag without the v (1.0.0 in our example).

Once the master pipeline runs, the version will be automatically published.

### I pushed the *master* branch BEFORE the tag, what now?

Don't worry, you can just trigger the CircleCI pipeline manually again for master even **after** pushing the tag.
