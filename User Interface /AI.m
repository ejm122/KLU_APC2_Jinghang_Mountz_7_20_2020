function index = AI(l,r) 
   index = (l-r) ./ (abs(l) + abs(r));
   %index = (l-r)./(0.5*(l+r));
end