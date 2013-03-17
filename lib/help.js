/*
 * Copyright 2011 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you
 * may not use this file except in compliance with the License. You
 * may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * Authors: klao@google.com, napszel@gmail.com, gergely@risko.hu
 */
hoplax.help =

'<h1 class="help">Welcome to Hoplax!</h1>'

+ '<p class="help"><a href="#"></a>To exit this help anytime, just type in the input box '
+ 'on the top of the page or click the logo!</p>'

+ '<p class="help">Hoplax is a power user tool to store bookmarks and '
+ 'interactive bookmarks (called shortcut bookmarks in Firefox) in '
+ 'text files.  Some advantages of Hoplax compared to '
+ 'bookmarks built into the browser:</p>'

+ '<ul class="help">'
+ '<li>Easy to edit and search with normal Unix tools and editors</li>'
+ '<li>Easy to synchronize with git/svn/darcs/etc.</li>'
+ '<li>Smarter autocompletion system</li>'
+ '<li>Interactive bookmarks with multiple parameters and completion</li>'
+ '<li>Different category of bookmarks in different files (e.g. company, personal)</li>'
+ '<li>Can be opened in as many instances as you want, unlike browser profiles</li>'
+ '</ul>'

+ '<p class="help">HOPLAX IS A WEBAPP, BUT STILL, IT IS NOT '
+ 'DESIGNED FOR USAGE WITH MOUSE, ALWAYS TRY TO STICK TO YOUR KEYBOARD. '
+ 'IF YOU HAVE TO USE YOUR MOUSE FOR SOMETHING, REPORT A BUG.</p>'

+ '<h2 class="help">The autocompletion system</h2>'

+ '<p class="help">Anytime you are in an input field that has autocompletion data you '
+ 'will see a table of the current completions and this table will '
+ 'change as you are typing.  You can use the cursor keys to wander in '
+ 'this table and choose another value.  If you are not happy with '
+ 'either of the possibilities the top choice is always your own '
+ 'input.  You can use Enter to accept anything from the table.</p>'

+ '<p class="help">The filtering of the table is based on the words you are '
+ 'typing in.  Each word has to match, non-matching rows '
+ 'will be excluded.  On the other hand the matching is relaxed, so '
+ 'you can skip some characters when you are typing a word.  E.g. in '
+ 'the maps autocompletion you can simply type guibi to get '
+ 'Guinea-Bissau.</p>'

+ '<p class="help">With large lists of possible completions this kind of matching '
+ 'results in many hits.  Fortunately, you do not have to worry about '
+ 'this, because we have a wonderful scoring system, so your intended '
+ 'target (hopefully) will always be one of the firsts.  Just bear '
+ 'with us and ignore the irrelevant results at the bottom.</p>'

+ '<h2 class="help">Mailing list and project homepage</h2>'

+ '<p class="help">You are invited to '
+ '<a href="http://code.google.com/p/hoplax">visit us</a>, '
+ '<a href="http://www.github.com/hoplax/hoplax">read and edit our code</a> '
+ 'and <a href="https://groups.google.com/group/hoplax">discuss</a>!</p>';

if (window.location.href.indexOf("http") == 0) {
  hoplax.help +=
    '<h2 class="help">Installing on local disk</h2>'

  + '<p class="help">Currently you are using Hoplax from the web, '
  + 'maybe from a demo instance (<a href="http://hoplax.github.com/">http://hoplax.github.com/</a>). '
  + 'Please be advised that local installation '
  + 'of Hoplax results in a much shorter load time, no bandwidth usage, more '
  + 'features.  Fortunately installation is very easy.  Just run '
  + '<tt>git clone git://github.com/hoplax/hoplax.git</tt> in an new, empty '
  + 'directory, then <tt>cp hoplax/conf_example.js conf.js</tt> and you are '
  + 'ready to browse your shiny file:// based installation.</p>'

  + '<p class="help">Once your installation is ready, you will be able to '
  + 'add your own bookmarks and develop your own plugins.</p>'

  + '<h2 class="help">Information below is relevant once you have installed Hoplax on your local disk</h2>';
}

hoplax.help +=
   '<h2 class="help">Storage of stuff, directory layout</h2>'

+ '<p class="help">You have unpacked Hoplax in a directory and after that '
+ 'you had to create the file conf.js at the same level '
+ 'in the filesystem with some default content.</p>'

+ '<p class="help">This was a one time installation procedure, from now on '
+ 'you can just keep on overwriting the hoplax directory with new versions and '
+ 'everything will be fine, your conf.js will be safe.</p>'

+ '<h2 class="help">Adding your bookmarks</h2>'

+ '<p class="help">Create a new file (e.g. userbookmarks.js) to store '
+ 'your bookmarks, use userbookmarks_example.js as a template.  Then include '
+ 'this file in conf.js.  If you have multiple set of bookmarks '
+ '(work, private, hobby), you can create multiple files and import '
+ 'only the relevant ones in the conf.js on each machine where you do work.</p>'

+ '<p class="help">In the conf.js you can also import Hoplax modules or '
+ 'other people\'s bookmark lists.  Please keep in mind that all the imports '
+ 'are relative to the main index.html.  You may also use absolute imports '
+ 'starting with file:/// or http://.  Please note, that file:/// is only '
+ 'allowed if your Hoplax installation is also local.</p>'

+ '<p class="help">Instead of modifying conf.js it is also possible to just '
+ 'add ?l=http://mysite.com/mybookmarks.js to the end of an hoplax URL to '
+ 'load mybookmarks.js.  This way you can customize your Hoplax without '
+ 'local installation, perfect if you are not working on your own computer. '
+ 'But please note that loading time will suffer and generally you are advised '
+ 'to keep a local copy.  Of course you should backup your data regularly!</p>'

+ '<h2 class="help">Importing your bookmarks</h2>'

+ '<p class="help">There is an app for that!  It is in tools/import-bookmarks.hs '
+ 'and that is not a typo, it is Haskell, not Shell.  No worries, we know that '
+ 'normal people do not have the GHC Haskell compiler installed (even if they '
+ 'should), so you can just download GNU/Linux binary at '
+ '<a href="http://github.com/hoplax/hoplax/tree/binaries">http://github.com/hoplax/hoplax/tree/binaries</a>. '
+ 'Ask Chrome/Firefox bookmark organizer to take a backup/export (choose JSON in '
+ 'Firefox!) and then <tt>./import-bookmarks &lt;backup.json &gt;mybookmarks.js</tt>. '
+ 'With Chrome you would be using <tt>backup.html</tt> of course.  Then just add '
+ 'mybookmarks.js to your Hoplax config and you are done.'

+ '<h2 class="help">Creating interactive bookmarks</h2>'

+ '<p class="help">You can also create interactive bookmarks (Hoplax plugins), '
+ 'they are easy.  Have a look on the Google Maps example in builtins.js.</p>'

+ '<p class="help">If you have created anything interesting, request a merge on '
+ 'Github!</p>';
