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
 * Authors: klao@google.com, napszel@gmail.com, errge@google.com
 */
hoplax.help =

'<tr><td><h1 class="help">Welcome to Hoplax!</h1>'

+ '<p class="help"><a href="#"></a>To exit this help anytime, just type in the input box '
+ 'on the top of the page!</p>'

+ '<p class="help">Hoplax is a power user tool to store bookmarks and '
+ 'interactive bookmarks (called shortcut bookmarks in Firefox) in '
+ 'text files.  Let us show some advantages of Hoplax compared to '
+ 'bookmarks built into the browser:</p>'

+ '<ul class="help">'
+ '<li>Easy to edit and search with normal Unix tools and editors</li>'
+ '<li>Easy to synchronize with git/svn/darcs/etc.</li>'
+ '<li>Smarter autocompletion system</li>'
+ '<li>Interactive bookmarks with multiple parameters and completion</li>'
+ '<li>Different category of bookmarks in different files (e.g. company, personal)</li>'
+ '<li>Can be opened in as many instances as you want, unlike browser profiles</li>'
+ '</ul>'

+ '<h2 class="help">Storage of stuff, directory layout</h2>'

+ '<p class="help">You have unpacked Hoplax in a directory and after that '
+ 'you were forced to create the file conf.js at the same level '
+ 'in the filesystem with some default content.</p>'

+ '<p class="help">This was a one time fiasco, from now on you can just keep on '
+ 'overwriting the hoplax directory with new versions and '
+ 'everything will be fine, your conf.js will be safe.</p>'

+ '<p class="help">In the conf.js you can import modules and bookmark lists into '
+ 'Hoplax.  Please keep in mind that all the imports are relative '
+ 'to the main index.html.  You may also use absolute imports.</p>'

+ '<h2 class="help">Adding your bookmarks</h2>'

+ '<p class="help">Create a new file (e.g. userbookmarks.js) beside conf.js to store '
+ 'your bookmarks, use userbookmarks_example.js as a template.  Do not '
+ 'forget to include this file in conf.js.  If you have multiple set '
+ 'of bookmarks, then you can create multiple files and only include '
+ 'the relevant files in the conf.js on each machine where you do work.'

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

+ '<h2 class="help">Importing your bookmarks</h2>'

+ '<p class="help">There is something in tools/import-bookmarks-jsonhtml.hs and we '
+ 'plan to ship binaries of this tool, but currently the tool is '
+ 'outdated and needs some love.  Sorry.  As always, help is welcome!</p>'

+ '<h2 class="help">Creating interactive bookmarks</h2>'

+ '<p class="help">You can also create interactive bookmarks, they '
+ 'are easy.  Have a look on the IMDb example in builtins.js.</p>'

+ '<p class="help">If you have created anything interesting, send it '
+ 'to us!</p>'

+ '<h2 class="help">Mailing list and project homepage</h2>'

+ '<p class="help">You are invited to '
+ '<a href="http://code.google.com/p/hoplax">visit us</a>, '
+ '<a href="http://www.github.com/hoplax/hoplax">read and edit our code</a> '
+ 'and <a href="https://groups.google.com/group/hoplax">discuss</a>!</p></td></tr>';
