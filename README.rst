Nim playground front-end
========================

This is the front-end for the Nim playground, built in Nim using `karax <https://github.com/pragmagic/karax>`_.  

The back-end code can be found at https://github.com/PMunch/nim-playground.

Compiling
---------

To build the frontend, first clone this repository  

.. code-block:: 
  
  git clone https://github.com/PMunch/nim-playground-frontend
  
then enter the directory and compile the frontend
  
.. code-block:: ruby
  
  cd nim-playground-frontend
  nim js -d:release -o:./assets/frontend.js ./frontend.nim
  
  
Finally, serve `index.html`.
