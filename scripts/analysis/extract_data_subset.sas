/* ========================================================================== */
/* NHIS 코호트 데이터 추출 SAS 스크립트                                       */
/* 목적: R에서 생성한 샘플 대상자 목록(label_obesity_severity_part.csv)을     */
/*       이용하여 대용량 테이블(M20, M30, M40, M60)에서 해당 대상자의         */
/*       데이터만 추출하여 CSV로 저장                                         */
/* ========================================================================== */

/* 1. 경로 설정 (사용자 환경에 맞게 수정 필요) */
%let path = /folders/myfolders/nhis-cohort-analysis/data;       /* SAS 데이터셋(.sas7bdat)이 있는 경로 */
%let outpath = /folders/myfolders/nhis-cohort-analysis/data/output; /* 출력 경로 */

/* 라이브러리 할당 */
libname rawdata "&path";

/* 2. 대상자 목록 로드 (R에서 생성한 파일 - CSV) */
proc import datafile="&outpath/label_obesity_severity_part.csv"
    out=target_list
    dbms=csv
    replace;
    getnames=yes;
run;

/* RN_INDI만 남기고 정렬 */
data target_indi;
    set target_list(keep=RN_INDI);
run;

proc sort data=target_indi nodupkey;
    by RN_INDI;
run;

/* 3. 데이터 추출 매크로 정의 */
%macro extract_table(table_name);
    
    /* 필터링 (PROC SQL 사용) */
    /* rawdata 라이브러리의 테이블을 직접 사용 */
    proc sql;
        create table &table_name._subset as
        select a.*
        from rawdata.&table_name a
        inner join target_indi b
        on a.RN_INDI = b.RN_INDI;
    quit;

    /* 결과 내보내기 (CSV로 저장하여 R에서 사용) */
    proc export data=&table_name._subset
        outfile="&outpath/&table_name._subset.csv"
        dbms=csv
        replace;
    run;
    
    /* 메모리 정리 */
    proc datasets library=work nolist;
        delete &table_name._subset;
    quit;

%mend extract_table;

/* 4. 각 테이블에 대해 매크로 실행 */
/* M20: 진료내역 */
%extract_table(M20);

/* M30: 진료내역 세부 */
%extract_table(M30);

/* M40: 상병내역 */
%extract_table(M40);

/* M60: 처방전내역 */
%extract_table(M60);

/* 완료 메시지 */
data _null_;
    file print;
    put "=================================================";
    put " 모든 테이블 추출이 완료되었습니다.";
    put " 저장 경로: &outpath";
    put "=================================================";
run;
