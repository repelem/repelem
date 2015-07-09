## Copyright (C) 2015 Markus Bergholz <markuman@gmail.com>
## Copyright (C) 2015 Nick Jankowski <jankowskin@asme.org>
##
## This program is free software; you can redistribute it and/or modify it under
## the terms of the GNU General Public License as published by the Free Software
## Foundation; either version 3 of the License, or (at your option) any later
## version.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

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
        ret = element(:,ones(v,1))'(:);
      elseif isrow(element)
        ret = element(ones(v,1),:)(:)';
      else
        error("%gD Array objects requires %g input arguments, only %g given",ndims(element),ndims(element)+1,nargin);
      endif
      
    elseif isvector(element)&&(length(v) == length(element))
    # this assumes a vector. but a 2x2 array and v = [2 2] would pass, but should error out.
    
      if isvector(element)
        # in any case, transform into row vector
        Delement  = element(:)';
        Dv        = v(:)';
        
        ret = arrayfun(@(Delement,Dv) repmat(Delement,1,Dv), Delement, Dv, 'UniformOutput', false);
        ret = [ret{:}];
        
        if iscolumn(element)
          # it was a column vector
          ret = ret';
        endif
      
      endif
      
    else
      error("varargin{1} must be a scalar or the same length as element")
    endif
    
  elseif (nargin > 2)
         
    # ret = arrayfun(@(element,N,M) repmat(element,N,M), element, N, M, 'UniformOutput', false);
    ret = arrayfun(@(element,varargin) repmat(element,varargin{:}), element, varargin{:}, 'UniformOutput', false);
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
