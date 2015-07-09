## Copyright (C) 2015 Markus Bergholz <markuman@gmail.com>
## Copyright (C) 2015 Nick Jankowski <jankowskin@asme.org>
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not,
## see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} repelem (@var{X}, @var{R})
## Construct an array of repeated elements from X.
##
## @seealso{cat korn repmat}
## @end deftypefn


function ret = repelem(element, varargin)

  if (nargin == 1)
    error("Not enough input arguments")
    
  elseif (nargin == 2)
  
    v = varargin{1};
    
    if (isscalar(v))
            
      if iscolumn(element)
        ret = element(:, ones(v, 1))'(:);
      elseif isrow(element)
        ret = element(ones(v, 1), :)(:)';
      else
        error("%gD Array objects requires %g input arguments, only %g given", ndims(element), ndims(element) + 1, nargin);
      endif
      
    elseif isvector(element) && (length(v) == length(element))
    # this assumes a vector. but a 2x2 array and v = [2 2] would pass, but should error out.
    
      # works for row or column vector. output direction will match element
      idx1 = cumsum(v); # gets ending position for each element item
      idx2(1:idx1(end)) = 0; # row vector with enough space for output
      idx2(idx1(1:end - 1) + 1) = 1; # sets starting position of each element to 1
      idx2(1) = 1; # sets starting position of each element to 1
      ret = element(cumsum(idx2)); # cumsum fills array with element indices is right position. element(cumsum) fills with element values, direction matches element.
  
      
    else
      error("varargin{1} must be a scalar or the same length as element")
      
    endif
    
  elseif (nargin > 2)
  
    ## INPUT CHECK
    elsize = ndims(element);
    nonscalarv = ~cellfun(@isscalar, varargin);
    
    # first check for valid number of inputs (compatibility: ML allows for unlimited trailing singletons)
    if ((nargin - 1) < elsize) %will allow for trailing singletons
      error("%gD Array objects requires %g input arguments, only %g given", ndims(element), ndims(element) + 1, nargin);

    # 2nd that they are all scalars or vectors. isvector gives true for scalars.
    elseif (~all(cellfun(@isvector, varargin))) 
      error("varargin must be all be scalars or vectors");
    
    # third, that the ones that are vectors have the right length. this should catch any vectors thrown at trailing singletons, which should only have scalars.
    elseif (~all(cellfun(@length, varargin(nonscalarv)) == size(element)(nonscalarv)))
      error("varargin(n) must either be scalar or have the same number of elements as the size of dimension n of the array to be replicated");        
      
    endif 
         
    ret = arrayfun(@(element, varargin) repmat(element, varargin{:}), element, varargin{:}, 'UniformOutput', false);
    ret = cell2mat(ret);
    
    ## TODO
    ## repeat column/rows n times
        
  
  endif

endfunction

%!assert (repelem([-1 0 1], 2), [-1 -1 0 0 1 1])
%!assert (repelem([-1 0 1]', 2), [-1; -1; 0; 0; 1; 1;])
%!assert (repelem([-1 0 1], [1 2 1]), [-1 0 0 1]) 
%!assert (repelem([-1 0 1]', [1 2 1]), [-1; 0; 0; 1])
%!assert (repelem([1 0;0 -1], 2, 3),  [1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1])
%!assert (repelem([1 0;0 -1], 2, 3, 4), cat(3,[1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1],[1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1],[1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1],[1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1]))
