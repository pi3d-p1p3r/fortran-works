#include <iostream>
using namespace std;

 int main(){
    int NUMBER_OF_ROWS = 4;
    int NUMBER_OF_COLUMNS=4;
    int matrix[NUMBER_OF_ROWS][NUMBER_OF_COLUMNS]={((1),(2),(3),(4)),((5),(6),(7),(8)),((9),(10),(11),(12)),((13),(14),(15),(16))};
    int row,col,largest;
    for (row = 0; row < NUMBER_OF_ROWS; row++){
        largest = matrix[row][0];

        for(col = 1; col<NUMBER_OF_COLUMNS; col++){
            if(largest<matrix[row][col])
            largest = matrix[row][col];            
        }
        cout << "The largest element in row " << row + 1 << " = " << largest << endl;
    }

    for (col = 0; col < NUMBER_OF_COLUMNS; col++){
        largest = matrix[0][col];

        for(row = 1; row<NUMBER_OF_ROWS; row++){
            if(largest<matrix[row][col])
            largest = matrix[row][col];
        }
        cout << "The largest element in column " << col + 1 << " = " << largest << endl;
    }

    return 0;
 }