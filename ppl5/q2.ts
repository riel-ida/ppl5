// q2.ts

export interface TreeNode {
  children: Tree[];
};

export interface TreeLeaf {
  value: number;
};

export type Tree = TreeNode | TreeLeaf;
export const isTreeNode = (x: any): x is TreeNode => x.children !== undefined;
export const isTreeLeaf = (x: any): x is TreeLeaf => x.value !== undefined;
export const isTree = (x: any): x is Tree => isTreeNode(x) || isTreeLeaf(x);

// Example values:

export const t1: Tree = {value: 5};
export const t2: Tree = {children: [
                   {children: [{value: 1}, {value: 7}, {value: 5}]},
                   {value: 3},
                   {value: 10}]};
export const t3: Tree = {children: [
                   {children: [{value: 20}, {value: 5}, {value: 50}]},
                   {value: 5}]};

export const leftMostEven1 = (atree: Tree): number => {
  if(isTreeLeaf(atree)){
    if(atree.value % 2 == 0) return atree.value;
    return -1;
  }
  //must be a treeNode
  //isEmpty?
  if(atree.children.length == 0 || atree.children == null) return -1;
  let asnCar = leftMostEven1(atree.children[0]);
  if(asnCar != -1) return asnCar;
  //case ansCar = -1
  let cdrTree: TreeNode = {children: atree.children.slice(1)};
  return leftMostEven1(cdrTree);
}
//test leftMostEven1
//console.log(leftMostEven1(t1));
//console.log(leftMostEven1(t2));
//console.log(leftMostEven1(t3));
//console.log(leftMostEven1({children: []}));

export const leftMostEven2 = (atree: Tree): number =>
  leftMostEven$(atree,
                (x) => x,
                () => -1);


const leftMostEven$ = <T1, T2>(atree: Tree,
                               succ: ((x:number) => T1),
                               fail: (() => T2)): (T1 | T2) =>{
    if(isTreeLeaf(atree)){
      if(atree.value % 2 == 0) return succ(atree.value);
      return fail();
    }
    //must be a treeNode
    //isEmpty?
    if(atree.children.length == 0 || atree.children == null) return fail();
    return leftMostEven$(atree.children[0], succ, () => {let cdrTree:TreeNode = {children:atree.children.slice(1)};
                                                          return leftMostEven$(cdrTree, succ, fail);});
}

//test CPS leftMostEven
//console.log(leftMostEven2(t1));
//console.log(leftMostEven2(t2));
//console.log(leftMostEven2(t3));
//console.log(leftMostEven2({children:[]}));


