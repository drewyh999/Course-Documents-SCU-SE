#include <stdlib.h>
#include <string.h>
#include "debugmalloc.h"
#include "dmhelper.h"
#include <stdio.h>

typedef struct block_node{
    void *blockaddr;
    int size_in_byte;
    char* filename;
    int linenumber;
    struct block_node *next;
}block_node;

block_node* allocated = NULL;
int header_size = 4 * sizeof(int) + sizeof(char*);

 int GetOnes(long long source,int size){

    int result = 0;
    size = size * 8;
    while(size > 0){
        if((source & 0x1) == 1){
            result ++;
        }
        source  = source >> 1;
        size --;
    }
    return result;
}

/* Wrappers for malloc and free */

void *MyMalloc(size_t size, char *filename, int linenumber) {
    unsigned int fence = 0xCCDEADCC;
    int sideway_size = header_size + sizeof(unsigned int);
    //GetCheckSum

    int Checksum_4 = GetOnes((long long)filename, sizeof(char*));
    int Checksum_3 = GetOnes(linenumber,sizeof(int));
    int Checksum_2 = GetOnes(size,sizeof(int));
    int Checksum_1 = GetOnes(fence,sizeof(int));

    int Checksum = Checksum_1 + Checksum_2 + Checksum_3 + Checksum_4;
    void *result = malloc(size + sideway_size);
    if(result == NULL){
        printf("malloc allocating failed");
        return NULL;
    }
    block_node *newnode = (block_node*) malloc(sizeof(block_node));
    if(newnode == NULL){
        printf("malloc allocating failed");
        return NULL;
    }
    newnode -> blockaddr = result;
    newnode -> size_in_byte = size;
    newnode -> filename = filename;
    newnode -> linenumber = linenumber;
    newnode -> next = NULL;
    if(allocated == NULL){
        allocated = newnode;
    }
    else{
        newnode -> next = allocated;
        allocated = newnode;
    }


    *(int*)result = Checksum;//写入cs
    //result = (char *)result + sizeof(int);
    result = ((int*)result + 1);

    *(char **)result = filename;//写入filename
    result = (char *)result + sizeof(char*);

    *(int*)result = linenumber;//写入linenumber
//    result = (char *)result + sizeof(int);
    result = ((int*)result + 1);


    *(int*)result = size;//写入size
//    result = (char *)result + sizeof(int);
    result = ((int*)result + 1);


    *(unsigned int*)result = fence;//fence at head
//    result = (char *)result + sizeof(int);
    result = ((int*)result + 1);


    /* head struct: ------checksum(4)filename(4)linenumber(4)size()fence(4)-----*/

    void *tail = result;
    tail = (char*) tail + size;
   // printf("sizeofint is %d",sizeof(unsigned int));
    *(int*)tail = fence; // fence at tail
    return result;
}

void MyFree(void *ptr, char *filename, int linenumber) {
    block_node *temp = allocated;
    unsigned int a = 0xCCDEADCC;
//    printf("size of long long is %d",GetOnes(0xffffffff,4));
    while(temp != NULL){
        if(((char *)(temp -> blockaddr) +  header_size) == ptr){
            void *head_start = temp -> blockaddr;
            int Checksum_1 = GetOnes(*(long long*)((char*)head_start + sizeof(int)), sizeof(char*));
            int Checksum_2 =GetOnes(*(int*)((char*)head_start + (sizeof(int) + sizeof(char*))), sizeof(int));
            int Checksum_3 =GetOnes(*(int*)((char*)head_start + (2 * sizeof(int) + sizeof(char*))), sizeof(int));
            int Checksum_4 =GetOnes(*(int*)((char*)head_start + (3 * sizeof(int) + sizeof(char*))), sizeof(int));
            int Checksum_sum = Checksum_1 + Checksum_2 + Checksum_3 + Checksum_4;
            if(Checksum_sum != *(int *)head_start){
                //Checksum changed?
                error(3,filename,linenumber);
                return;
            }
            if(*(unsigned int*)((char*)head_start + (3 * sizeof(int) + sizeof(char*))) != 0xCCDEADCC){//Convert the pointer to unsigned int* then get the value
                //head fence changed?
                errorfl(1,temp -> filename,temp -> linenumber, filename,linenumber);
            }
            if((*(unsigned int*)((char*)head_start + (4 * sizeof(int) + sizeof(char*)) + temp -> size_in_byte)) != 0xCCDEADCC){
                errorfl(2,temp -> filename,temp -> linenumber, filename,linenumber);//tail fence changed?
            }
            free(temp->blockaddr);
            temp -> blockaddr = NULL;
            block_node *p = allocated;
            if(p == temp){
                free(temp);
                p = NULL;
                temp = NULL;
                allocated = NULL;
                return;
            }
            while(p -> next != temp){
                p = p -> next;
            }
            p -> next = p -> next -> next;
            free(temp);
            temp = NULL;
            return;
        }
        temp = temp -> next;
    }
    error(4,filename,linenumber);
}

/* returns number of bytes allocated using MyMalloc/MyFree:
	used as a debugging tool to test for memory leaks */
int AllocatedSize() {
    block_node *temp = allocated;
    int result = 0;
    while(temp != NULL){
        result += temp ->size_in_byte;
        temp = temp -> next;
    }
	return result;
}



/* Optional functions */

/* Prints a list of all allocated blocks with the
	filename/line number when they were MALLOC'd */
void PrintAllocatedBlocks() {
    block_node *temp = allocated;
    while(temp != NULL){
        printf("Currently allocated blocks: %d bytes, created at %s, line %d",temp -> size_in_byte, temp -> filename, temp -> linenumber);
        temp = temp -> next;
    }
	return;
}

/* Goes through the currently allocated blocks and checks
	to see if they are all valid.
	Returns -1 if it receives an error, 0 if all blocks are
	okay.
*/
int HeapCheck() {
    block_node *temp = allocated;
    while(temp != NULL){
        void *head_start = temp -> blockaddr;
        int Checksum_1 = GetOnes(*(long long*)((char*)head_start + sizeof(int)), sizeof(char*));
        int Checksum_2 =GetOnes(*(int*)((char*)head_start + (sizeof(int) + sizeof(char*))), sizeof(int));
        int Checksum_3 =GetOnes(*(int*)((char*)head_start + (2 * sizeof(int) + sizeof(char*))), sizeof(int));
        int Checksum_4 =GetOnes(*(int*)((char*)head_start + (3 * sizeof(int) + sizeof(char*))), sizeof(int));
        int Checksum_sum = Checksum_1 + Checksum_2 + Checksum_3 + Checksum_4;
        if(Checksum_sum != *(int *)head_start){
            //Checksum changed?
            PRINTERROR(3,temp -> filename,temp -> linenumber);
            return -1;
        }
        if(*(unsigned int*)((char*)head_start + (3 * sizeof(int) + sizeof(char*))) != 0xCCDEADCC){//Convert the pointer to unsigned int* then get the value
            //head fence changed?
            PRINTERROR(1,temp -> filename,temp -> linenumber);
            return -1;
        }
        if((*(unsigned int*)((char*)head_start + (4 * sizeof(int) + sizeof(char*)) + temp -> size_in_byte)) != 0xCCDEADCC){
            PRINTERROR(2,temp -> filename,temp -> linenumber);//tail fence changed?
            return -1;
        }
        temp = temp -> next;
    }
	return 0;
}
