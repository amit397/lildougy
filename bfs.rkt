;Create a bfs function that goes through a tree and returns whether or not a integer exists within a specific tree

;Design Idea: TO create a bfs function we need a way to iterate through the tree in level order traversal, we will do this by creating list that we will call a q, where we will add the left and right children
;of a node of the tree and add it back to the list q to be processed. The bfs function will first, however, check if its is at the bottom of the tree by checking whether or not q is null then assigning the variable node
;to represent the car of q and rest to the cdr of q (rest of the list). This is where the function will check if the current node is the integer (n) we are looking for, if so returen #t, if not then check if there is another
;level to the tree on the current node using pair?, if so then add the next level (left and right nodes) to rest and process the other nodes in q using another call of bfs, if not just process the rest of the q.

;Guess Invariant: bfs(tree, target) = (node.value == target) or bfs(node.left, target) or bfs(node.right, target)

(define (bfs q n)
  (if (null? q)
      #f
      (let ((node (car q))
            (rest (cdr q)))
        (if (eq? node n)
            #t
        (if (pair? node)
            (let ((left (cadr node))
                  (right (caddr node)))
              (bfs (append rest (list left right)) n))
            (bfs rest n))))))

(bfs '(13 (5 6 1) (45 7 18)) 19)

;Precondition: q must be a tree (a list of lists in scheme) where each node (element within the list of lists) must be a integer and n must be a integer since this will be our target value and our data types must match
;with the tree structure.

;Weak Enough: During the first iteration of the function bfs it will check if the value of the current node (car q) is equivalent to the target, if so it will return #t, and if not it will process the left and the right
;childs, which will be the rest of the tree, to check whether the target value is there

;-> better way of writing WE:


;Strong Enough: During the last iteration the function will be on the last node of the tree where it will check whether or not the node value (car q) is equivalent to the target if so it will return #t, if not it will 
;to call the function again with its left and right childss (which are null) where bfs will detect that the q has become null and it will return #f

;Preservability: During one of the middle calls of the function bfs all the previously processed nodes must not have contained the target value so if our target value is detected during our current node value we will
;return #t and if it doesn't then process the left and right children of the current node until either the target value is encountered or the end of the tree is reached

;Termination Condition: Either the target value is found within one of the nodes in the tree or the end of the tree is reached at which point the current value of the node as well as the left and right children are null
;making the invariant return #f.

;Postcondition: Return either #t or #f if the target value n in encountered within the tree