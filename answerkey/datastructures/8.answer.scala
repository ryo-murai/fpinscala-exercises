/* 
No, this is not possible! The reason is that _before_ we ever call our function, `f`, we evaluate its argument, which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation to support early termination---we discuss this in a later chapter.  
*/