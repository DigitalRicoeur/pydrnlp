# pydrnlp #
## NLP Library for Digital Ricoeur ##

Copyright © 2018–present Digital Ricoeur contributors

This repository contains:

1. A Python 3 package providing some NLP functionality needed
   by Digital Ricoeur; and

2. A Racket package that manages the Python-implemented
   functionality and integrates it with other Digital Ricoeur tools.

The initial goal is to support the NLP functionality needed
to implement our own versions of some of the widgets that
Digital Ricoeur currently gets from Voyant.

The Python portion of this repository is in the `py/` directory,
particularly the `py/pydrnlp/` directory.
It is built on the `spaCy` NLP library.
A `conda` executable (e.g. from Miniconda) must be in your PATH,
but Racket takes care of creating and updating the virtual
environment as needed.

For more information, see the documentation built from
`scribblings/pydrnlp.scrbl`.

The canonical version of this repository is at
<https://bitbucket.org/digitalricoeur/pydrnlp>.
There is also a publish-only mirror at
<https://github.com/DigitalRicoeur/pydrnlp>.


## License ##

This repository contains software and documentation developed for
Digital Ricoeur (<https://digitalricoeur.org>).
It is made available to the public under free and open-source licenses:

-   Permission is granted to use, redistribute, and/or modify the software in
    this repository under the terms of the GNU Affero General Public License
    as published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this software. If not, see <https://www.gnu.org/licenses/>.

-   The documentation (not software) in this repository is licensed
    under the Creative Commons Attribution-ShareAlike 4.0 International
    License (CC BY-SA).
    To view a copy of this license, visit
    <https://creativecommons.org/licenses/by-sa/4.0/>.
