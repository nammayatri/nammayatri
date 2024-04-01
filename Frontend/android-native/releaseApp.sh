var1=$1

# Use a case statement to match var1 against patterns
case $var1 in
  "NY")
    bundle exec fastlane release_ny
    ;;
  "MY")
    bundle exec fastlane release_my
    ;;
  "Y")
    bundle exec fastlane release_y
    ;;
  *)
    bundle exec fastlane
    ;;
esac