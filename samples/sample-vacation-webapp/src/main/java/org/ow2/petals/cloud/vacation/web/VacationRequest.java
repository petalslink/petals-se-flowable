/**
 * Copyright (c) 2015-2018 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.cloud.vacation.web;

import java.util.Date;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

import org.hibernate.validator.constraints.NotEmpty;
import org.springframework.format.annotation.DateTimeFormat;

/**
 * 
 * @author vnoel
 *
 */
public abstract class VacationRequest {

    @Min(0)
    private long dayNumber = 1;

    @DateTimeFormat(pattern = "yyyy-MM-dd")
    private Date startDate = new Date();

    /**
     * Can't be null or empty if not flowable complains the variable does not exists...
     */
    @NotNull
    @NotEmpty
    private String reason;

    private String id;

    private String enquirer;

    public String getEnquirer() {
        return enquirer;
    }

    public void setEnquirer(String enquirer) {
        this.enquirer = enquirer;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public long getDayNumber() {
        return dayNumber;
    }

    public void setDayNumber(long dayNumber) {
        this.dayNumber = dayNumber;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public String getReason() {
        return reason != null ? reason : "";
    }

    public void setReason(String reason) {
        this.reason = reason;
    }

    public static class PendingVacationRequest extends VacationRequest {

    }

    public static class RefusedVacationRequest extends VacationRequest {

        private String rejectionReason;

        public String getRejectionReason() {
            return rejectionReason != null ? rejectionReason : "";
        }

        public void setRejectionReason(String rejectionReason) {
            this.rejectionReason = rejectionReason;
        }
    }

    public static class ArchivedVacationRequest extends RefusedVacationRequest {

        private boolean accepted;

        public boolean getAccepted() {
            return this.accepted;
        }

        public void setAccepted(final boolean accepted) {
            this.accepted = accepted;
        }

    }
}
