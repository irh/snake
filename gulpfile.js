var gulp = require('gulp');
var jshint = require('gulp-jshint');
var concat = require('gulp-concat');
var uglify = require('gulp-uglify');
var rename = require('gulp-rename');
var elm = require('gulp-elm');


gulp.task('elm-init', elm.init);

gulp.task('elm-make', ['elm-init'], function(){
  return gulp.src('src/main.elm')
    .pipe(elm.make({filetype: 'js'}))
    .pipe(gulp.dest('build'));
});

gulp.task('lint', function() {
  return gulp.src('src/*.js')
    .pipe(jshint())
    .pipe(jshint.reporter('default'));
});

gulp.task('build', ['lint', 'elm-make'], function() {
  return gulp.src(['build/*.js', 'src/*.js'])
    .pipe(concat('snake.js'))
    .pipe(gulp.dest('dist/js'))
    .pipe(rename('snake.min.js'))
    .pipe(uglify())
    .pipe(gulp.dest('dist/js'));
});

gulp.task('watch', function() {
  gulp.watch('src/*.elm', ['build']);
  gulp.watch('src/*.js', ['build']);
});

gulp.task('default', ['build', 'watch']);
