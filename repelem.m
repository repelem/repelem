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
## @deftypefnx {Function File} {} repelem (@var{X}, @var{R1}, @dots{}, @var{Rn})
## Construct an array of repeated elements from X.
##
## @var{X} must be a scalar, a vector or an array.
##
## @var{Rn} must either be scalar or have the same number of elements as the 
## size of dimension n of the array to be replicated.
##
## @seealso{cat korn repmat}
## @end deftypefn


function ret = repelem(element, varargin)

  if (nargin <= 1)
    error("repelem: Not enough input arguments")
    
  elseif (nargin == 2)
  
    v = varargin{1};
    
    if (isscalar(v))
            
      if (iscolumn(element))
        # element values repeated v times in a col vector
        ret = element(:, ones(v, 1))'(:); 
      elseif (isrow(element))
        # element values repeated v times in a row vector
        ret = element(ones(v, 1), :)(:)'; 
      else
        error("repelem: %gD Array objects require %g or more input arguments, only %g given", ndims(element), ndims(element) + 1, nargin);
      endif
      
    elseif (isvector(element) && (length(v) == length(element)))
      # vector element with vector varargin. basic run-length decoding in function prepareIdx
      # returned idx2 has a row vect. of element indices in right position
      idx2 = prepareIdx(v);
      # fills with element values, direction matches element. 
      ret  = element(idx2);
        
    else
      error("repelem: varargin{1} must be a scalar or the same length as element")
      
    endif
  
  
  elseif (nargin == 3)  #can simplify for known dimension count
    
    #avoid repeated function calls
    elsize = size(element);
    scalarv = cellfun('numel', varargin)==1;  #'numel' or 'length' faster than isvector in cellfun
    
    ##INPUT CHECK

    # check: that all varargin are either scalars or vectors, no arrays. isvector gives true for scalars.
    # (Faster here with only two to avoid cellfun)
    if (~(isvector(varargin{2})&&isvector(varargin{2}))) 
      error("repelem: varargin must be all scalars or vectors");

    # check that the ones that are vectors have the right length.
    elseif (any(~(cellfun('length', varargin(~scalarv)) == elsize(~scalarv))))
      error("repelem: varargin(n) must either be scalar or have the same number of elements as the size of dimension n of the array to be replicated");        
    endif 
    
    #Create index arrays to pass to element 
    ##(no slower passing to prepareIdx than checking and doing scalars directly)
    idx1 = prepareIdx(varargin{1},elsize(1));
    idx2 = prepareIdx(varargin{2},elsize(2));
  
    #the : at the end takes care of size(element)>2
    ret = element(idx1, idx2, :); 

  else  #if (nargin > 3) **no need for elseif
  
    # avoid repeated function calls
    eldims = ndims(element);
    elsize = size(element);
    vasize = numel(varargin);
    maxDim = max([eldims vasize]);
    minDim = min([eldims vasize]);
    dims_with_both = min(eldims, vasize);
    nonscalarv = ~cellfun(@isscalar, varargin);

    ## INPUT CHECK

    # 1st check: that they are all scalars or vectors. isvector gives true for scalars.
    if (~all(cellfun(@isvector, varargin))) 
      error("repelem: varargin must be all be scalars or vectors");
    
    # 2nd check: catch any vectors thrown at trailing singletons, which should only have scalars.
    elseif (max(find(nonscalarv)) > eldims)
      error("repelem: varargin(n) for trailing singleton dimensions must be scalar");        
    
    # 3rd check: that the ones that are vectors have the right length.
    elseif (~all(cellfun('length', varargin(nonscalarv)) == size(element)(nonscalarv)))
      error("repelem: varargin(n) must either be scalar or have the same number of elements as the size of dimension n of the array to be replicated");        
      
    endif 
    
    # first, preallocate idx which will contain index array to be put into element
    idx = cell(1, maxDim);

    # use prepareIdx() to fill indices for each dimension that could be a scalar or vector
    idx(1:minDim) = cellfun(@prepareIdx, varargin(1:minDim), num2cell(elsize(1:minDim)), 'UniformOutput', false);

    # then, if there are fewer varargin inputs than element dimensions, pad 
    # remaining dimensions with [1:size(el,n)], essentially leaving that dim alone
    if (eldims > vasize)  
      idx(vasize+1:eldims) = cellfun(@colon, {1}, num2cell(elsize(vasize + 1:end)), 'UniformOutput', false);
    
    # if instead there are more varargin inputs than element dimensions, 
    # add [1 1 1 1 1... 1] to those dims to effect concatenation in those dims.
    elseif (vasize > eldims)
      idx(eldims + 1:vasize) = cellfun(@ones, {1}, {varargin(eldims + 1:end){:}}, 'UniformOutput', false);
    
    endif
    
    # use completed idx to specify repitition of element values in all dimensions
    ret = element(idx{:});
  
  endif

endfunction


function idx2 = prepareIdx(v, elsize_n)
# returns a row vector of indices prepared for replicating.

  if (isscalar(v))
    # will always return row vector
    idx2 = [1:elsize_n](ones(v, 1), :)(:)'; 

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
%!error  (repelem([1 2 3; 3 2 1], 1, [1 2]))
%!error  (repelem([1 2 3; 3 2 1], [1 2 2 1]))
%!error  (repelem([1 2 3; 3 2 1]))
%!error  (repelem([1 2 3; 3 2 1],2))
%!error  (repelem([1 2 3; 3 2 1],[1 2 3]))
%!error  (repelem([1 2 3; 3 2 1],[1 2 3;4 5 6]))