%   Call conflict. for check any conflict. If there are any
% conflict, this query return true; otherwise return false.
%   You can also call conflicts separately like:
%      courseConflict.
%      roomConflict.
%      studentConflict.
%      instructorConflict.
%
%   Call assignRoomToClass(R, C). for check which room can be
% assigned to which class or check which room can be assigned
% to given class. R for room, C for class.
%
%   Call enrollStudent(S, C). for check wheter a student can
% be enrolled to a given class or check which classes a
% student can be assigned. S for student, C for class.
%
%   Currently:
%     cse343-cse339 and cse221-cse339 cause room conflict
%
%     cse331-cse339 and cse221-cse442 cause student conflict
%     155-cse442-z10 cause student conflict due to handicape
%
%     cse442, cse331 and cse221 cause course conflict:
%        cse442: course capacity bigger than room capacity
%        cse331: course need projector but room does not have
%        cse221: enrolled students more than capacity
%
%     kaya and bayrakci cause instructor conflict:
%        kaya: instructor need projector but room does not have
%        bayrakci: instructor teach cse339 and cse229 but time
%        of these courses overlap
%
%
%
%    NOTE : I used occupancy fact that hold course, room and
%    course hours. Occupancy is used to detect time conflicts.
%    So if you add new courses, you may want to add the
%    relative occupancy fact to make detecting time conflicts
%    consider new addeds better. You can add occupancy with 
%    addOccupancy(R,C,H) where R is room id, C is course id 
%    and H is hours as list (e.g. [9, 10, 11] for 3 hours course).
%
%

:- dynamic student/3.
:- dynamic room/5.
:- dynamic course/8.
:- dynamic occupancy/3.

%ID, Courses, Handicape
student(155, [cse343, cse442], 1).
student(156, [cse221], 0).
student(157, [cse331, cse339], 0).
student(158, [cse229], 0).
student(159, [cse221, cse442], 0).
student(160, [cse101], 0).
student(180, [], 0).

%ID, Courses, Projector, Smartboard
instructor(genc, [cse343, cse221], 1, 0).
instructor(akgul, [cse442], 1, 1).
instructor(kaya, [cse331], 1, 0).
instructor(bayrakci, [cse339, cse229], 1, 1).
instructor(ali, [cse101], 0, 0).

%ID, Capacity, Projector, Smartboard, Handicape
room(z06, 40, 1, 1, 1).
room(z10, 30, 1, 1, 0).
room(z11, 10, 0, 0, 0).
room(z20, 150, 1, 1, 1).

%ID, Instructor, Capacity, Hours, Room, Enrolleds, Projector, Smartboard
course(cse343, genc, 20, 2, z06, [155], 1, 1).
course(cse442, akgul, 35, 3, z10, [159, 155], 1, 1).
course(cse101, ali, 10, 2, z11, [160], 0, 0).
course(cse221, genc, 1, 4, z06, [156, 159], 1, 0).
course(cse331, kaya, 10, 2, z11, [157], 1, 0).
course(cse339, bayrakci, 30, 3, z10, [157], 1, 1).
course(cse229, bayrakci, 150, 6, z20, [158], 1, 1).

%RoomID, CourseID, Hours
occupancy(z06, cse343, [8, 9]).
occupancy(z06, cse221, [10, 11, 12, 13]).
occupancy(z10, cse442, [11, 12, 13]).
occupancy(z11, cse331, [9, 10]).
occupancy(z11, cse101, [11, 12]).
occupancy(z20, cse229, [11, 12, 13, 14, 15, 16]).
occupancy(z06, cse339, [9, 10, 11]).

%ID, Courses(as list), Handicape(1 or 0)
addStudent(I, C, H):-
    assertz(student(I, C, H)).

% ID, Instructor, Capacity, Hour, Room, Enrolled(as list),
% Projector(1 or 0), Smartboard(1 or 0)
addCourse(I, T, C, H, R, E, P, S) :-
    assertz(course(I, T, C, H, R, E, P, S)).

% ID, Capacity, Projector(1 or 0), Smartboard(1 or 0),
% Handicape Access(1 or 0)
addRoom(I, C, P, S, A) :-
    assertz(room(I, C, P, S, A)).

addOccupancy(R, C, H) :-
    assertz(occupancy(R, C, H)).

checkAnyHandicape(E) :-
    member(X, E),
    student(X, _, 1).

handicapeRoom(E, RH) :-
    (   checkAnyHandicape(E) ->
           1 =:= RH
    ;
          true
    ).

assignRoomToClass(R, C) :-
    course(C, I, S, _, _,E, P, B),
    instructor(I, _, IP, IB),
    room(R, RC, RP, RB, RH),
    handicapeRoom(E, RH),
    S =< RC, %course capacity <= room cap.
    P =< RP, %if course need projector
    B =< RB, %if course need smartboard
    IP =< RP, %if instructor need projector
    IB =< RB. %if instructor need smartboard

% used for student enrollment
noNewTimeConflict(E, C):-
    (   member(X, E),
        occupancy(_, X, BusyHours),
        occupancy(_, C, NewHours),
        member(H1, BusyHours),
        member(H2, NewHours),
        H1 =:= H2->
           false
    ;
          true
    ).

enrollStudent(S, C) :-
    student(S, EnrolledCourses, H),
    course(C, _, CC, _, R, E, _, _),
    room(R, _, _, _, RH),
    H =< RH,
    length(E, L),
    L < CC,
    noNewTimeConflict(EnrolledCourses, C).

% if a student is handicaped and one of the enrolled course
% room has not access for the handicaped
studentConflict :-
    student(_, Courses, 1),
    member(C, Courses),
    course(C, _, _, _, R, _, _, _),
    room(R, _, _, _, 0).

% if a student is enrolled two courses that overlap
studentConflict :-
    student(_, Courses, _),
    member(C1, Courses),
    member(C2, Courses),
    C1 \== C2,
    occupancy(_, C1, Hours1),
    occupancy(_, C2, Hours2),
    member(H, Hours1),
    member(H, Hours2).

% if one of the speacial needs of a course or capacity
% is not afforded
courseConflict :-
    course(_, _, C, _, R, _, P, S),
    room(R, RC, RP, RS, _),
    (RP < P;
    RS < S;
    RC < C).

% if amount of enrolled students is bigger than capacity
courseConflict :-
    course(_, _, C, _, _, E, _, _),
    length(E, L),
    C < L.

% if two course at same time on same room
roomConflict :-
    occupancy(R, C1, Hours1),
    occupancy(R, C2, Hours2),
    C1 \== C2,
    member(H, Hours1),
    member(H, Hours2).

% if one of the preferences of instructor is not afforded
instructorConflict :-
    instructor(_, Courses, P, S),
    member(C, Courses),
    course(C, _, _, _, R, _, _, _),
    room(R, _, RP, RS, _),
    (RP < P;
    RS < S).

% if courses of instructor are scheduled to same time
instructorConflict :-
    instructor(_, Courses, _, _),
    member(C1, Courses),
    member(C2, Courses),
    C1 \== C2,
    occupancy(_, C1, Hours1),
    occupancy(_, C2, Hours2),
    member(H, Hours1),
    member(H, Hours2).

conflict :-
    studentConflict;
    courseConflict;
    roomConflict;
    instructorConflict.
