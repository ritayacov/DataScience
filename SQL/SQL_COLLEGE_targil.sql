/*תרגיל להגשה*/
/*1.	יצירת בסיס נתונים וטבלאות - a. b. c.*/

  SELECT [CourseId]
      ,[StudentId]
      ,[degree]
  INTO dbo.Classrooms_orig
  FROM [COLLEGE].[dbo].[Classrooms]
  where [CourseId] IS NOT NULL AND [StudentId] IS NOT NULL AND [degree] IS NOT NULL

  select * from dbo.Classrooms_orig

select [CourseId]
      ,[CourseName]
      ,[DepartmentID]
      ,[TeacherId]
	INTO  dbo.COURSES_orig
  FROM [COLLEGE].[dbo].[Courses]
  where [CourseId] IS NOT NULL AND [CourseName] IS NOT NULL AND [DepartmentID] IS NOT NULL AND [TeacherId] IS NOT NULL ;

  SELECT [DepartmentId]
      ,[DepartmentName]
  INTO dbo.Departments_orig
  FROM [COLLEGE].[dbo].[Departments]
  where DepartmentId IS NOT NULL AND DepartmentName  IS NOT NULL

   SELECT [StudentId]
      ,[FirstName]
      ,[LastName]
      ,[Gender]
 INTO dbo.Students_orig
  FROM [COLLEGE].[dbo].[Students]
  where [StudentId] IS NOT NULL AND [FirstName] IS NOT NULL AND [LastName] IS NOT NULL AND [Gender] IS NOT NULL

  SELECT [TeacherId]
      ,[FirstName]
      ,[LastName]
      ,[Gender]
INTO dbo.TEACHERS_orig
  FROM [COLLEGE].[dbo].[Teachers]
  where [TeacherId] IS NOT NULL AND [FirstName] IS NOT NULL AND [LastName] IS NOT NULL AND [Gender] IS NOT NULL

/*שאלה 2*/
/*a.	מנהל המכללה ביקש לדעת כמה סטודנטים יש לפי יחידה (מחלקה).*/

  select distinct a.StudentId,
		 b.DepartmentID,
		 c.DepartmentName 
into #STUDENTS_DEPAR
  FROM dbo.Classrooms_orig as a
	inner join dbo.COURSES_orig as b ON (a.CourseId=b.CourseId)
	inner join dbo.Departments_orig as c ON (b.DepartmentID=c.DepartmentId)


select distinct DepartmentID,
	  DepartmentName,
	count(distinct StudentId) as STUDENT_num
		into #STUD_DEPAR_cnt
	from #STUDENTS_DEPAR
GROUP BY DepartmentName, DepartmentID

select * from #STUDENTS_DEPAR
select * from #STUD_DEPAR_cnt

 /*b and c*/

 select distinct a.StudentId,
		 b.DepartmentID,
		 c.DepartmentName,
		 b.CourseId,
		 b.CourseName 
into #STUDENTS_English
  FROM dbo.Classrooms_orig as a
	inner join dbo.COURSES_orig as b ON (a.CourseId=b.CourseId)
	inner join dbo.Departments_orig as c ON (b.DepartmentID=c.DepartmentId)
where DepartmentName='English'

select * from #STUDENTS_English

select distinct CourseName ,
	  CourseId,
	count(distinct StudentId) as STUDENT_num,
	(select count( distinct StudentId) from #STUDENTS_English) as Total_English_stud,
	(case when (count(distinct StudentId))<22 then 'small class' else ' big class' end) as class_size  
		into #STUD_DEPAR_English
	from #STUDENTS_English
GROUP BY CourseId, CourseName 

select * from #STUD_DEPAR_English



/*d*/

select GENDER, 
count(distinct StudentId) as Student_num
	into #Students_Gender
from dbo.Students_orig
	group by GENDER

select * from #Students_Gender

/*e.באיזה קורסים אחוז הגברים / הנשים הינה מעל 70%?*/

select CourseId, count(StudentId) as Total_stud_cl into #Total_stud_class FROM dbo.Classrooms_orig group by CourseId

select * from #Total_stud_class

select distinct a.CourseId,
				b.CourseName,
				c.Gender,
				count(a.StudentId) as Student_cnt,
				d.Total_stud_cl
	into #STUDENTS_course
  FROM dbo.Classrooms_orig as a
	left join dbo.COURSES_orig as b on (a.CourseId=b.CourseId)
	left join dbo.Students_orig as c ON (a.StudentId=c.StudentId)
	left join  #Total_stud_class as d ON (a.CourseId=d.CourseId)
	group by a.CourseId, b.CourseName, c.Gender,d.Total_stud_cl 


select * from #STUDENTS_course

select CourseId,
	   CourseName,
		Gender,
	(Student_cnt*1.0/Total_stud_cl*1.0) as Gender_per
  into #STUD_cour_Gen
from #STUDENTS_course

select * from #STUD_cour_Gen

/*f.	בכל אחד מהיחידות (מחלקות), כמה סטודנטים (מספר ואחוזים) עברו עם ציון מעל 80?*/


/*סך סטודנטים בקורס ומחלקה*/

select distinct a.DepartmentID, a.CourseId, count(b.StudentId) as Std_dpr_crs_cnt 
					into #Total_std_dpr_crs
				from dbo.COURSES_orig as a	
					left join dbo.Classrooms_orig as b on (a.CourseId=b.CourseId) group by a.DepartmentID, a.CourseId

select * from #Total_std_dpr_crs

/*חישוב מספר ואחוז סטודנטים לפי קבוצת ציונים*/

select distinct a.DepartmentID,
	   c.DepartmentName,
	   a.CourseId,
	   a.CourseName,
	   count(b.StudentId) as Std_cnt,
	   d.Std_dpr_crs_cnt,
	   (case when d.Std_dpr_crs_cnt<>0 then (count(b.StudentId)*1.0/d.Std_dpr_crs_cnt*1.0) else 0 end) as Std_cnt_prc,
	   (case when b.degree>80 then 'more then 80' else 'less then 80' end) as degree_class
from dbo.COURSES_orig as a	
	left join dbo.Classrooms_orig as b on (a.CourseId=b.CourseId)
	left join dbo.Departments_orig as c on (a.DepartmentID=c.DepartmentId)
	left join  #Total_std_dpr_crs as d 
					on (a.DepartmentID=d.DepartmentId) AND (a.CourseId=d.CourseId)
	where (case when b.degree>80 then 'more then 80' else 'less then 80' end)='more then 80'
group by a.DepartmentID, c.DepartmentName, a.CourseId, a.CourseName, d.Std_dpr_crs_cnt,
(case when b.degree>80 then 'more then 80' else 'less then 80' end)
order by a.DepartmentID, c.DepartmentName, a.CourseId, a.CourseName, d.Std_dpr_crs_cnt

/*g.	בכל אחד מהיחידות (מחלקות), כמה סטודנטים (מספר ואחוזים) לא עברו (ציון מתחת ל-60) ?  */

select distinct a.DepartmentID,
	   c.DepartmentName,
	   a.CourseId,
	   a.CourseName,
	   count(b.StudentId) as Std_cnt,
	   d.Std_dpr_crs_cnt,
	   (case when d.Std_dpr_crs_cnt<>0 then (count(b.StudentId)*1.0/d.Std_dpr_crs_cnt*1.0) else 0 end) as Std_cnt_prc,
	   (case when b.degree>60 then 'more then 60' else 'less then 60' end) as degree_class
from dbo.COURSES_orig as a	
	left join dbo.Classrooms_orig as b on (a.CourseId=b.CourseId)
	left join dbo.Departments_orig as c on (a.DepartmentID=c.DepartmentId)
	left join  #Total_std_dpr_crs as d 
					on (a.DepartmentID=d.DepartmentId) AND (a.CourseId=d.CourseId)
	where (case when b.degree>60 then 'more then 60' else 'less then 60' end)='less then 60'
group by a.DepartmentID, c.DepartmentName,a.CourseId, a.CourseName,d.Std_dpr_crs_cnt,
(case when b.degree>60 then 'more then 60' else 'less then 60' end)
order by a.DepartmentID, c.DepartmentName,a.CourseId, a.CourseName, d.Std_dpr_crs_cnt

/*h.	תדרגו את המורים לפי ממוצע הציון של הסטודנטים מהגבוהה לנמוך.*/

select distinct a.TeacherId,
		avg(b.degree) as Degree_avg
	INTO #AVG_Degree_teacher
from dbo.COURSES_orig as a	
	left join dbo.Classrooms_orig as b on (a.CourseId=b.CourseId)
group by a.TeacherId
order by a.TeacherId, avg(b.degree) desc  


SELECT TeacherId,
	Degree_avg,
	RANK () OVER ( 
		ORDER BY Degree_avg DESC
	) teacher_rank 
FROM
	#AVG_Degree_teacher;

/*שאלה 3*/

/*a.	תייצרו VIEW המראה את הקורסים, היחידות (מחלקות) עליהם משויכים, המרצה בכל קורס ומספר התלמידים רשומים בקורס*/
create view teachers_courses_v as
	select distinct a.CourseId,
	   a.CourseName,
		a.DepartmentID,
		c.DepartmentName,
	   	   a.TeacherId,
	   count(b.StudentId) as Std_cnt
from dbo.COURSES_orig as a	
	left join dbo.Classrooms_orig as b on (a.CourseId=b.CourseId)
	left join dbo.Departments_orig as c on (a.DepartmentID=c.DepartmentId)
group by a.CourseId, a.CourseName, a.DepartmentID, c.DepartmentName, a.TeacherId

/*b.	תייצרו VIEW המראה את התלמידים, מס' הקורסים שהם לוקחים,הממוצע של הציונים לפי יחידה (מחלקה) והממוצע הכוללת שלהם.*/

select StudentId, count(CourseId) as CRS_cnt, avg(degree) Degree_avg_total INTO dbo.std_crs_degree from dbo.Classrooms_orig group by StudentId

create view std_crs_grades_v as
	select distinct a.StudentId,
			d.CRS_cnt,
			b.DepartmentID,
			c.DepartmentName,
		    avg(a.degree) as Dergee_avg_dpr,
	   	   d.Degree_avg_total
from dbo.Classrooms_orig as a	
	left join dbo.COURSES_orig as b on (a.CourseId=b.CourseId)
	left join dbo.Departments_orig as c on (b.DepartmentID=c.DepartmentId)
	left join dbo.std_crs_degree as d on (a.StudentId=d.StudentId)
group by a.StudentId, d.CRS_cnt, b.DepartmentID, c.DepartmentName, d.Degree_avg_total

