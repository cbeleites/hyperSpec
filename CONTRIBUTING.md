:+1::tada: First off, thanks for taking the time to contribute! :tada::+1:


The following is a set of guidelines for contributing to the `R` package `hyperSpec`.

## Code Licensing
You understand that your work becomes the part of the `hyperSpec` project and is going to be licensed under the [GNU GPL v3](https://github.com/cbeleites/hyperSpec/blob/master/LICENSE).


## Reporting Bugs and Submitting Suggestions

* Ensure the bug was not already reported by searching on GitHub under [Issues](https://github.com/cbeleites/hyperSpec/issues).

* If you're unable to find an open issue addressing the problem, open a new one. Be sure to include a title and clear description, as much relevant information as possible, and a code sample or an executable test case demonstrating the expected behavior that is not occurring.
Refer to the [Bug Reporting in R article](https://www.r-project.org/bugs.html) for guidance.


## Code and Documentation Styleguide
As agreed upon in #96, this project adheres to the [Tidyverse styleguide](https://style.tidyverse.org/).

This guide applies both to the code and the [`roxygen2` documentation](https://style.tidyverse.org/documentation.html).

Use package [styler](http://styler.r-lib.org/) with RStudio add-in to easily re-style your code to comply with the guidelines.


## Working With Git

### Branches
This project follows the [`git flow` branching model](https://nvie.com/posts/a-successful-git-branching-model/).

<img src='https://nvie.com/img/git-model@2x.png' width='400px'>

#### Briefly

The branch `master` contains stable releases that are tested and guaranteed to work. It is not allowed to contribute directly to `master`.

The branch `develop` contains latest delivered development changes for the next release. When `develop` reaches a stable point and is ready to be released, it gets merged to `master` and tagged with a version number (e.g. 'v0.99.21'). This procedure is a subject for a strict review.

You *should not* directly contribute to `develop`, unless the change is trivial (e.g. a typo).
Instead, for any new feature or bugfix, please create a separate supporting branch. We use a default naming convention for them:

* `feature/###-<feature_name>` for new features, where `###` is the corresponding [issue number](https://github.com/cbeleites/hyperSpec/issues).
* `bugfix/###-<bugfix_name>` for bugfixes
* `release/x.y.z` for release preparation, where `x.y.z.` is the version to be released.

It is recommended to use the `git flow` tool to streamline the process (see [Cheatsheet for git flow](https://danielkummer.github.io/git-flow-cheatsheet/)). However, do not call `git flow xxx finish` as it makes a merge *without* the code review - instead, [finish your branch by opening a pull request](https://softwareengineering.stackexchange.com/a/189062/302312).

#### Wait, What if I'm not Allowed to Create a Branch in the Main Repository?

If you are not a member of the project then you cannot create a branch in the main repository. But this is not a problem! In this case, you simply fork the main repository, make the changes starting off the `develop` branch, and merge it back into the `develop` branch of the main repository via a pull request.

After a successfull code review the pull request gets accepted, and your changes are represented in the main repo as a separate branch (in accordance with our guidelines). After that you can delete your fork, if you'd like.

### Pull Requests

Open a pull request via GitHub interface to let others see your work and review it. It is a collaborative tool, so we encourage you to open a ['draft pull request'](https://github.blog/2019-02-14-introducing-draft-pull-requests/) as soon as you start working on your part. This provides a place for the community to discuss your work and correct it as you go. Once your part is completed, change the status to “Ready for review”.

> The project maintainer may want to merge your pull request when your work is usable, even before it is 100% complete. In such a case, the brunch must be deleted and a new one created off the `develop`. You can use the same name for it to indicate that this is a continuation. It will be merged, as usual, via a new pull request. This may seem to be an overhead at first glance, but it leads to a faster integration and makes the the pull requests smaller and less overwhelming.

The merged support branches [should be deleted - they're clutter](https://ardalis.com/why-delete-old-git-branches). If you want to keep their name for reference, just apply a `git tag` after the merge. Never reuse merged branches, [it can lead to problems](https://stackoverflow.com/a/29319178).


### Git Commits

Commit often, try to make small atomic commits. An atomic commit addresses only a small separate fix or change and is more or less self-consistent. Every commit should be related to one feature only.

### Commit Messages
* Use the present tense ("Add feature" not "Added feature")
* Use the imperative mood ("Move cursor to..." not "Moves cursor to...")
* Limit the first line to 72 characters or less
* Reference issues and pull requests liberally after the first line


## Versioning

The project adheres to the semantic versioning guidelines, as outlined at https://semver.org/.

Thanks! :heart:
