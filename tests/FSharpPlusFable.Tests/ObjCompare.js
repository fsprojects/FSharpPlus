function objCompare(objA, objB){

    //Check to see if both parameters are strictly equal
    if (objA === objB){
        return;
    }
    
    //If both parameters are of different types, then they are not equal
    if (typeof objA !== typeof objB){
        throw "First parameter has type " + typeof objA + " whilst second parameter has type " + typeof objB;
    }
    //Checks whether both parameters are arrays and if the contents of them are equal
    else if (Array.isArray(objA) && Array.isArray(objB)){

        if (objA.length !== objB.length){
            throw "First array has length " + objA.length + " whilst second array has length " + objB.length;
        };
        objA.every((e, i) => objCompare(e, objB[i]));
    }
    //Check whether both parameters are objects
    else if (typeof objA === "object"){
        //Try to get the keys of both parameters
        const propA = Object.keys(objA);
        const propB = Object.keys(objB);
        
        //Both parameters can't be true if they are different lengths
        if (propA.length !== propB.length){
            throw "First parameter has length " + propA.length + " whilst second parameter has length " + propB.length;
        }

        //Checks if the values within both parameters are equal
        propA.forEach(prop => objCompare(objA[prop], objB[prop]));
    }
    else{

    //All other cases means both parameters are not equal
    throw "First parameter " + objA + " and second parameter " + objB + " are not equal";
    }

}

module.exports = {

    EqualObj: objCompare
}
