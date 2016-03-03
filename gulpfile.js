var gulp = require('gulp');
var jshint = require('gulp-jshint');
var concat = require('gulp-concat');
var uglify = require('gulp-uglify');
var rename = require('gulp-rename');
var elm = require('gulp-elm');
var audiosprite = require('gulp-audiosprite');
var concat = require('gulp-concat-util');
var ghPages = require('gulp-gh-pages');

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
  gulp.src(['build/*.js', 'src/*.js'])
    .pipe(concat('snake.js'))
    .pipe(gulp.dest('dist/js'))
    .pipe(rename('snake.min.js'))
    .pipe(uglify())
    .pipe(gulp.dest('dist/js'));
  gulp.src(['src/*.html'])
    .pipe(gulp.dest('dist'));
});

gulp.task('watch', ['build'], function() {
  gulp.watch('src/*.elm', ['build']);
  gulp.watch('src/*.js', ['build']);
});

gulp.task('sounds', function() {
  gulp.src('sounds/*.wav')
    .pipe(audiosprite({
      export: 'mp3,ac3',
      format: 'howler',
      gap: 0.5,
      path: 'sounds/'
    }))
    .pipe(gulp.dest('build/sounds'));
  gulp.src('build/sounds/sprite.json')
    .pipe(concat('sprite.json'))
    .pipe(concat.header('var soundSprite = '))
    .pipe(concat.footer(';'))
    .pipe(gulp.dest('dist/sounds'));
  gulp.src('build/sounds/*.{mp3,ac3}')
    .pipe(gulp.dest('dist/sounds'));
})

gulp.task('default', ['build', 'sounds']);

gulp.task('deploy', function() {
  gulp.src('dist/**/*')
    .pipe(ghPages());
});
