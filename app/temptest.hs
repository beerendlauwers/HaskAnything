
import Data.List (intersect,nub)
import Data.Maybe (catMaybes)


tags = [ ("tag1",["path1","path2"]), ("tag2",["path1","path3"]), ("tag3",["path3"]) ]
categories = [ ("cat1",["path1"]), ("cat2",["path1","path2"]), ("cat3",["path3"]) ]


matchTagsWithCategories = 
 map f categories
  where 
  f (cat,paths) = (cat, nub $ catMaybes $ concatMap (\c -> map (find c) tags ) categories)
  

find (cat,cpaths) (tag,tpaths) = 
   if length (intersect cpaths tpaths) > 0
    then Just tag
    else Nothing
                               