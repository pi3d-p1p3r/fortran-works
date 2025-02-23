program a1q1
    implicit none
    integer :: class_no
    real :: mid_term, final_exam,class_test,score

    print*,"Enter the lcass no.,class test score, mid term score, final exam: "
    read(*,*)class_no, class_test,mid_term,final_exam
    score = class_no*0.25 + class_test*0.2 + mid_term*0.2 + final_exam*0.5

    if(score>90)then
        print*,"A"
    else if(score>80 .and. score<90)then
        print*,"B"
    else if(score>70 .and. score<80)then
        print*,"C"
    else if(score>60 .and. score<70)then
        print*,"D" 
    else if(score>0 .and. score<60)then
        print*,"F"
    else
        print*,"Invalid grade"
    end if

    case(90:100)
        print*,"A"
        print*,score
    case(80:89)
        print*,"B"
    case(80:89)
        print*,"B"
    case(80:89)
        print*,"B"
end program