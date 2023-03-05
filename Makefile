release:
	cd frontend; make
	cd backend; make

develop:
	cd frontend; make develop &
	cd backend; make develop
