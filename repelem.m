## Copyright (C) 2015 Markus Bergholz <markuman@gmail.com>
## Co-author  Nicholas R. Jankowski <jankowskin@asme.org> **
##   ** U.S. government employee, contributions are public domain
##      with no personal assertion of copyright
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
      #row or column vector, one scalar or vector replication input
      
    v = varargin{1};
    
    if (isscalar(v))
            
      if iscolumn(element)
        ret = element(:, ones(v, 1))'(:); #element values repeated v times in a col vector 
      elseif isrow(element)
        ret = element(ones(v, 1), :)(:)'; #element values repeated v times in a row vector 
      else
        error("%gD Array objects requires %g input arguments, only %g given", ndims(element), ndims(element) + 1, nargin);
      endif
      
    elseif isvector(element) && (length(v) == length(element))
      #vector element with vector varargin. basic run-length decoding in function prepareIdx
      idx2 = prepareIdx(v); #returned idx2 has a row vect. of element indices in right position
      ret  = element(idx2); #fills with element values, direction matches element.
  
      
    else
      error("varargin{1} must be a scalar or the same length as element")
      
    endif
    
  elseif (nargin > 2)
  
    ## INPUT CHECK
    
    #avoid repeated function calls
    eldims = ndims(element);
    elsize = size(element);
    vasize = numel(varargin);
    nonscalarv = ~cellfun(@isscalar, varargin);
    
    # 1st check: that they are all scalars or vectors. isvector gives true for scalars.
    if (~all(cellfun(@isvector, varargin))) 
      error("varargin must be all be scalars or vectors");
    
    # 2nd check: catch any vectors thrown at trailing singletons, which should only have scalars.
    elseif (max(find(nonscalarv)) > eldims)
      error("varargin(n) for trailing singleton dimensions must be scalar");        
    
    # 3rd check: that the ones that are vectors have the right length.
    elseif (~all(cellfun(@length, varargin(nonscalarv)) == size(element)(nonscalarv)))
      error("varargin(n) must either be scalar or have the same number of elements as the size of dimension n of the array to be replicated");        
      
    endif 
    
    #preallocate idx which will contain index array to be put into element
    idx = cell(1,1:max([eldims vasize]));

    # iterate over each dimension of element, or the number specified by varargin, whichever is larger
    for n = 1:max([eldims vasize])

    # for each n, use prepareIdx() to create a cell of indices for each dimension.
      if ((n <= eldims) && (n <= vasize)) # n is within varargin and eldims

        idx{1,n} = prepareIdx(varargin{n}, elsize(n)); %prepareIdx always returns row vect

      #if eldims ~= vasize, fill remaining cells according to which is bigger
      elseif (eldims > vasize)  
        idx{1,n} = 1:size(element,n); #will give [1,2,3,...,size(el,n)], for dims not covered by varargin, essentially leaving them alone
      else
        idx{1,n} = ones(1,varargin{n}); # will give [1 1 1 1 1 ... 1] for simple replication in the nth dimension for varagins addressing trailing singletons
          
      endif
    endfor
      
    ret = element(idx{:}); #use completed idx to specify repitition of element values in all dimensions
  
  endif

endfunction


function idx2 = prepareIdx(v, elsize_n)
  #returns a row vector of indices prepared for replicating.

  if (isscalar(v))

    idx2 = [1:elsize_n](ones(v,1),:)(:)';#will always return row vector

  else
  
    # works for row or column vector. idx2 output will be a row vector.
    idx1 = cumsum(v); # gets ending position for each element item
    idx2(1:idx1(end)) = 0; # row vector with enough space for output
    idx2(idx1(1:end - 1) + 1) = 1; # sets starting position of each element to 1
    idx2(1) = 1; # sets starting position of each element to 1
    idx2 = cumsum(idx2); #with prepared index
    
  endif
  
endfunction

%!assert (repelem([-1 0 1], 2), [-1 -1 0 0 1 1])
%!assert (repelem([-1 0 1]', 2), [-1; -1; 0; 0; 1; 1;])
%!assert (repelem([-1 0 1], [1 2 1]), [-1 0 0 1]) 
%!assert (repelem([-1 0 1]', [1 2 1]), [-1; 0; 0; 1])
%!assert (repelem([1 0;0 -1], 2, 3),  [1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1])
%!assert (repelem([1 0;0 -1], 2, 3, 4), cat(3,[1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1],[1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1],[1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1],[1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1]))
%!assert (repelem([1 0; 0 -1], 1,[3 2]), [1 1 1 0 0;0 0 0 -1 -1])
%!assert (repelem([1 0; 0 -1], 2,[3 2]), [1 1 1 0 0;1 1 1 0 0;0 0 0 -1 -1;0 0 0 -1 -1])
%!assert (repelem(cat(3,[1 0; 0 -1],[1 0;0 -1]), 1,[3 2]),repmat([1 1 1 0 0 ; 0 0 0 -1 -1],1,1,2))
%!assert (repelem([1 0; 0 -1], [3 2], 1), [1 0;1 0;1 0;0 -1;0 -1])
%!assert (repelem([1 0; 0 -1], [3 2], 2), [1 1 0 0;1 1 0 0;1 1 0 0;0 0 -1 -1;0 0 -1 -1])
%!assert (repelem([1 0; 0 -1], [2 3] ,[3 2]), [1 1 1 0 0;1 1 1 0 0;0 0 0 -1 -1;0 0 0 -1 -1;0 0 0 -1 -1])
%!assert (repelem(cat(3,[1 1 1 0;0 1 0 0],[1 1 1 1;0 0 0 1],[1 0 0 1;1 1 0 1]), 2, 3), cat(3,[1 1 1 1 1 1 1 1 1 0 0 0;1 1 1 1 1 1 1 1 1 0 0 0;0 0 0 1 1 1 0 0 0 0 0 0;0 0 0 1 1 1 0 0 0 0 0 0],[1 1 1 1 1 1 1 1 1 1 1 1;1 1 1 1 1 1 1 1 1 1 1 1;0 0 0 0 0 0 0 0 0 1 1 1;0 0 0 0 0 0 0 0 0 1 1 1],[1 1 1 0 0 0 0 0 0 1 1 1;1 1 1 0 0 0 0 0 0 1 1 1;1 1 1 1 1 1 0 0 0 1 1 1;1 1 1 1 1 1 0 0 0 1 1 1]))
%!assert (repelem(cat(3,[1 1 1 0;0 1 0 0],[1 1 1 1;0 0 0 1],[1 0 0 1;1 1 0 1]), 2, [3 3 3 3]),cat(3,[1 1 1 1 1 1 1 1 1 0 0 0;1 1 1 1 1 1 1 1 1 0 0 0;0 0 0 1 1 1 0 0 0 0 0 0;0 0 0 1 1 1 0 0 0 0 0 0],[1 1 1 1 1 1 1 1 1 1 1 1;1 1 1 1 1 1 1 1 1 1 1 1;0 0 0 0 0 0 0 0 0 1 1 1;0 0 0 0 0 0 0 0 0 1 1 1],[1 1 1 0 0 0 0 0 0 1 1 1;1 1 1 0 0 0 0 0 0 1 1 1;1 1 1 1 1 1 0 0 0 1 1 1;1 1 1 1 1 1 0 0 0 1 1 1]))
%!assert (repelem([1,0,-1;-1,0,1],[2 3],[2 3 4],2),cat(3,[1 1 0 0 0 -1 -1 -1 -1;1 1 0 0 0 -1 -1 -1 -1;-1 -1 0 0 0 1 1 1 1;-1 -1 0 0 0 1 1 1 1;-1 -1 0 0 0 1 1 1 1],[1 1 0 0 0 -1 -1 -1 -1;1 1 0 0 0 -1 -1 -1 -1;-1 -1 0 0 0 1 1 1 1;-1 -1 0 0 0 1 1 1 1;-1 -1 0 0 0 1 1 1 1]))