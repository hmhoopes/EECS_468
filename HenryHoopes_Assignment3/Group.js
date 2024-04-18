//Begins the declaration of prototype for Group objects
class Group{

    //declares constructor for group
    constructor(){
        //creates an empty array as a property for group objects
        this.arr = [];
    }

    //declares has function has for group that takes an element as an argument
    has(val){
        //returns if that element is in the object's array
        return this.arr.includes(val);
    }

    //declares function add for group that takes an element as argument
    add(val){
        //if the element isn't in the object's array, run the following
        if (!this.has(val)){
            //add element to the object's array
            this.arr.push(val);
        }
    }

    //declares function delete for group that takes an element as argument
    delete(val){
        //if the element is in the object's array, run the following
        if (this.has(val)){
            //get the index of the element
            let index = this.arr.indexOf(val);
            //use splice function to delete element from object's array
            this.arr.splice(index, 1);
        }
    }

    //declares function union for group that takes another group as argument
    union(other){
        //declares a new group object
        let newGroup = new Group();
        //uses spread operator to copy this object's array into new group's array
        newGroup.arr =[...this.arr];
        //uses forEach to add every element of other group's array to new group's array
        other.arr.forEach(element => newGroup.add(element));  
        //returns new group
        return newGroup;

    }

    //declares function intersection for group that takes another group as argument
    intersection(other){
        //declares new group object
        let newGroup = new Group();
        //uses forEach to operate with each element in this object's array
        this.arr.forEach(element => {
            //if the other group has element in its array, run the following
            if (other.has(element)){
                //adds element to new group
                newGroup.add(element);
            }
        });
        //returns new group
        return newGroup;
    }

    //declares function difference for group that takes another group as argument
    difference(other){
        //declares new group object
        let newGroup = new Group();
        //uses forEach to operate with each element in this object's array
        this.arr.forEach(element => {
            //if the other group doesn't have element in its array, run the following
            if(!other.has(element)){
                //adds element to new group
                newGroup.add(element);
            }
        });
        //returns new group
        return newGroup;
    }

}
