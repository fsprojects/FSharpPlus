# Branches

## Main

There is one main branch where most work ends up. Right now the name of that branch is master.

This means that you generally want to target this branch when contributing to F#+.

## V1

For the v1 release we have the following branches:

- [v1.1](https://github.com/fsprojects/FSharpPlus/tree/v1.1) containing the release branch for 1.1.* releases.
- [v1.2](https://github.com/fsprojects/FSharpPlus/tree/v1.2) containing the release branch for 1.2.* releases.
- [v1.3](https://github.com/fsprojects/FSharpPlus/tree/v1.3) release branch for 1.3.* releases. Note that this branch contains a merge from master instead of using cherry picking. This means that some of the changes in master have been **reverted** to avoid breaking changes.
- [v1.4](https://github.com/fsprojects/FSharpPlus/tree/v1.4) release branch for 1.4.* releases.
- [v1.5](https://github.com/fsprojects/FSharpPlus/tree/v1.5) release branch for 1.5.* releases. Note that we have done experiments to change the target framework net45 to net462 and found that it would be [a breaking change](https://github.com/fsprojects/FSharpPlus/tree/v1.5-net462-instead-of-net45).
- [v1.6](https://github.com/fsprojects/FSharpPlus/tree/v1.6) release branch for 1.6.* releases.

Most of these branches are only for reference purposes (can be safely ignored) and should not be touched. The important v1.x branch is the latest minor branch on this track. v1.1 - v1.5 should be stale, v1.6 might become stale once there is no need for any fixes in that minor version. 

The reason why we have used these branches has been due to the fact that we have had to diverge from master in order to keep compatibility with the version 1 release. The later versions in v1.1 and v1.2 releases was mostly cherry picked from master in order to release only the intended changes.
