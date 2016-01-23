module FPPrac.Trees 
    ( RoseTree(..)
    , RBTree(..)
    , NodeColor(..)
    , rbExampleTree
    , roseExampleTree
    , showRBTree
    , showRBTreeList
    , showRoseTree
    , showRoseTreeList
    , showTree
    , showTreeList
    ) where
    
    
import Prelude

import Eventloop.Core
import Eventloop.Types.Events
import Eventloop.Types.System
import Eventloop.DefaultConfiguration

import qualified Eventloop.Module.Websocket.Canvas as C
import Eventloop.Module.DrawTrees
import Eventloop.Module.BasicShapes
import Eventloop.Utility.Trees.GeneralTree

data ProgramState = ProgramState
                  deriving (Eq, Show)

beginProgramState = ProgramState

eventloopConfiguration trees
    = EventloopSetupConfiguration
        { beginProgstate = beginProgramState
        , eventloopF = eventloop trees
        , setupModuleConfigurations = [ setupDrawTreesModuleConfiguration
                                      , setupBasicShapesModuleConfiguration
                                      , C.setupCanvasModuleConfiguration
                                      ]
        }



eventloop :: [GeneralTree] -> ProgramState -> In -> (ProgramState, [Out])
eventloop trees state Start = (state, [ OutCanvas $ C.SetupCanvas 1 1 (round width, round height) (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))
                                      , OutDrawTrees $ DrawTrees 1 trees
                                      , Stop
                                      ]
                              )
                            where
                                (_, width, height) = showGeneralTreeList trees
                              
                             

showRBTree :: RBTree -> IO()
showRBTree tree = showTree tree
 
showRBTreeList :: [RBTree] -> IO ()
showRBTreeList trees = showTreeList trees


showRoseTree :: RoseTree -> IO()
showRoseTree tree = showTree tree
 
showRoseTreeList :: [RoseTree] -> IO ()
showRoseTreeList trees = showTreeList trees


showTree :: (GeneralizeTree a) => a -> IO ()
showTree tree = showTreeList [tree]

showTreeList :: (GeneralizeTree a) => [a] -> IO ()
showTreeList trees = startEventloopSystem (eventloopConfiguration $ map generalizeTree trees)