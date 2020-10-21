# Maintainer Manual

## Publishing (for maintainers)

To publish a release to Maven Central follow these steps:

1. Create a tag/release on GitHub
2. Publish the artifact to the OSS Sonatype stage repository:
   ```
   sbt +publishSigned
   ```  
   Note that your Sonatype credentials needs to be configured on your machine and you need to have access writes to publish artifacts to the group id `io.moia`.
3. Release artifact to Maven Central with:
   ```
   sbt +sonatypeRelease
   ```
