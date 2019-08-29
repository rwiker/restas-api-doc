# restas-api-doc
### _Raymond Wiker <rwiker@gmail.com>_

This is a simple RESTAS extension module to generate documentation for
a REST API, either for a specific HTTP endpoint, or for a specific
RESTAS module.

It works by finding documentation strings for modules (via their
package objects) and for all routes defined for the modules. Some
other information is also extracted and used, like the HTTP method and
content-type.

When this module is mounted, it presents one route that generates a
simple index page, as well as a page that presents information for a
specific route (determined by method and template).

Simple example:
```
(restas:mount-module api-doc (#:restas-api-doc)
  (:url "api-doc/")
  (:inherit-parent-context t))
```

This example is generic; it makes no assumptions about the current
RESTAS module, and the API documentation is extracted whenever the
"api-doc/" endpoint (index page) is accessed (and also whenever the
informastion for a specific route is requested).

Slightly more complex:
```
(restas:mount-module api-doc (#:restas-api-doc)
  (:url "api-doc/")
  (:inherit-parent-context t)
  (restas-api-doc:*doc-collection* 
    (restas-api-doc::collect-api-doc/module '#:my-server)))
```

This example extracts the API documentation at evaluation time, so
that it does not have to be done on every access to the "api-doc/"
endpoint.
 

## License

Copyright 2019 Raymond Wiker

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
