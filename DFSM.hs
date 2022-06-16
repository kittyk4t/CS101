module DFSM where

import Data.Set (Set) -- imports Set constructor for type

import qualified Data.Set as Set 
 -- imports operations, though must be prefixed with "Set."
 
{- implementation of arbitrary deterministic FSM.
   For simplicity, all states are just integers.  This
   implementation is more complex than the simple example because the
   transition function is not hard coded into the program.  I.e. the 
   new program can simulate any FSM.  Now when we make a transition,
   we must not just return the new state, but also carry along the
   transition function and the accepting states.t 

   Main program can be run by typing
      accept myDFSM inputString
   where myDFSM is a description of a deterministic FSM and inputString
   is the input.  The result is a boolean reflecting whether
   inputString is accepted or not.
-}

{- nextState converts a collection of triples into a transition
   function that determines next state from current state and input
   Each triple is of the form (s,a,t) where there is transition from
   s to t on input a.  Thus
       nextState trans currentState input  
   returns the state the machine will be in using the transition 
   function trans after reading input starting from currentState 
-}

nextState :: Eq st => [(st, Char, st)] -> st -> Char -> st

-- if no triple applies then throw exception
nextState [] curState letter = error ("no transition on "++[letter])
-- if find matching triple with curState and input, return associated
-- new state from the triple, otherwise keep looking
nextState ((s0,inp,s1):rest) curState input =
          if (curState == s0 && input == inp)
                then s1
                else nextState rest curState input

{- A finite state machine consists of the start state,
   the transition function, and the set of accepting states.
-}

data DFSM state = DFSM
    { startState         :: state
    , transitionFunction :: state -> Char -> state
    , acceptingStates    :: Set state
    }

makeDFSM
  :: Eq state => state -> [(state, Char, state)] -> Set state -> DFSM state
makeDFSM start transTriples acceptStates =
   DFSM{ startState = start,
        transitionFunction = nextState transTriples,
        acceptingStates = acceptStates
      }

{- gaccept fsm state input returns true iff the fsmm starting in state 
   goes into an accepting state in fsm.acceptingStates
   after reading in all input.
-}
gAccept :: Ord st => DFSM st -> st -> [Char] -> Bool
-- if no input left, see if state in accepting state of fsm
gAccept fsm s [] =  Set.member s (acceptingStates fsm)
-- to process next input, find new state from input,
-- and then run fsm  on rest of input
gAccept fsm s (fst:rest) = gAccept fsm s' rest where
        s' = (transitionFunction fsm) s fst

-- main function to determine if string accepted by fsm:
accept :: Ord state => DFSM state -> [Char] -> Bool
accept fsm input = gAccept fsm (startState fsm) input

------------ CODE TO TEST IMPLEMENTATION --------------

-- Transition function for DDFSM accepting strings containing aab.
tran2 = [(0,'a',1),(0,'b',0),(1,'a',2),
                           (1,'b',0),(2,'a',2),(2,'b',3),(3,'a',3),(3,'b',3)]

-- complete DDFSM corresponding to machine above
-- try it out by typing: accept aabDFSM "aabbbba"
aabDFSM = makeDFSM 0 tran2 (Set.singleton 3)
