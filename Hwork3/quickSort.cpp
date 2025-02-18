

#include <iostream>
#include <stack>
using namespace std;

int partition(int arr[], int low, int high) {  //Partition function for correct position of pivot
    int pivot = arr[high];
    int i = low - 1;

    for (int j = low; j < high; j++) { //swap the elements smaller than pivot

        if (arr[j] < pivot) {
            i++;
            swap(arr[i], arr[j]);
        }
    }

    swap(arr[i + 1], arr[high]);  //Swap pivot to its correct position
    return i + 1;
}

void quickSort(int arr[], int low, int high) {  // Use a stack to implement quickSort without recursion
    stack<int> lowStack, highStack;
    
    lowStack.push(low);
    highStack.push(high);
    
    while (!lowStack.empty()) {    //start while loop till the stack is empty
        low = lowStack.top();
        high = highStack.top();
        lowStack.pop();
        highStack.pop();
        
        if (low < high) { //sort both left and right side of array
            int pivot = partition(arr, low, high);
            
            if (pivot + 1 < high) {  //Push right subarray
                lowStack.push(pivot + 1);
                highStack.push(high);
            }
            if (low < pivot - 1) { //Push left subarray
                lowStack.push(low);
                highStack.push(pivot - 1);
            }
        }
    }
}

int main() {
    int arr[] = {10, 7, 8, 9, 1, 5};
    int sizeOfArray = sizeof(arr) / sizeof(arr[0]);  //Get the size of the array
    quickSort(arr, 0, sizeOfArray - 1);  //call quicksort
    
    cout << "Sorted array: ";  //output of sorted array
    for (int i = 0; i < sizeOfArray; i++)
        cout << arr[i] << " ";
    cout << endl;
    
    return 0;
}
