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
  "NYP")
    bundle exec fastlane release_nyp
    ;;
  "MYP")
    bundle exec fastlane release_myp
    ;;
  "YP")
    bundle exec fastlane release_yp
    ;;
  "OYP")
    bundle exec fastlane release_oyp
    ;;
  *)
    bundle exec fastlane
    ;;
esac