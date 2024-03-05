/**
 * Copyright (c) 2017-2024 Linagora
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
package org.ow2.petals.flowable.utils.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.regex.Pattern;

import org.flowable.engine.HistoryService;
import org.flowable.engine.ManagementService;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.TaskService;
import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.history.HistoricProcessInstanceQuery;
import org.flowable.engine.runtime.Execution;
import org.flowable.engine.runtime.ExecutionQuery;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.runtime.ProcessInstanceQuery;
import org.flowable.job.api.DeadLetterJobQuery;
import org.flowable.job.api.Job;
import org.flowable.task.api.Task;
import org.flowable.task.api.TaskQuery;
import org.flowable.task.api.history.HistoricTaskInstance;
import org.flowable.task.api.history.HistoricTaskInstanceQuery;

/**
 * Assertion about Flowable process events
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class Assert {

    /**
     * Assertion to check that a process instance is running. The process instance is not finished.
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param processDefinitionKey
     *            The process definition key
     * @param runtimeService
     *            The Flowable's runtime service used to request the Flowable engine about process instances.
     */
    public static void assertProcessInstancePending(final String processInstanceId, final String processDefinitionKey,
            final RuntimeService runtimeService) {

        final ProcessInstanceQuery processInstQuery = runtimeService.createProcessInstanceQuery();
        final ProcessInstance processInstance = processInstQuery.processInstanceId(processInstanceId).singleResult();
        assertNotNull(processInstance);
        assertEquals(processInstanceId, processInstance.getProcessInstanceId());
        assertEquals(processDefinitionKey, processInstance.getProcessDefinitionKey());
        assertFalse(processInstance.isEnded());
        assertFalse(processInstance.isSuspended());
    }

    /**
     * Assertion to check that a process instance is finished.
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param historyService
     *            The Flowable's history service used to request the Flowable engine about historic process instances.
     */
    public static void assertProcessInstanceFinished(final String processInstanceId,
            final HistoryService historyService) {

        final HistoricProcessInstanceQuery query = historyService.createHistoricProcessInstanceQuery();
        final HistoricProcessInstance processInstance = query.processInstanceId(processInstanceId).finished()
                .singleResult();
        assertNotNull(processInstance);
    }

    /**
     * <p>
     * Assertion to check that a single user task can be completed by a user.
     * </p>
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param taskDefinitionKey
     *            The process definition key
     * @param taskService
     *            The Flowable's task service used to request the Flowable engine about task instances.
     * @return The task to complete
     */
    public static Task assertCurrentUserTask(final String processInstanceId, final String taskDefinitionKey,
            final TaskService taskService) {

        final TaskQuery taskQuery = taskService.createTaskQuery();
        final Task nextTask = taskQuery.processInstanceId(processInstanceId).taskDefinitionKey(taskDefinitionKey)
                .singleResult();
        assertNotNull(nextTask);
        assertEquals(processInstanceId, nextTask.getProcessInstanceId());
        assertEquals(taskDefinitionKey, nextTask.getTaskDefinitionKey());

        return nextTask;
    }

    /**
     * <p>
     * Assertion to check that a single user task can be completed by the given user.
     * </p>
     * <p>
     * The requested user can be the task assignee, or a member of a candidate group, or a candidate user.
     * </p>
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param taskDefinitionKey
     *            The process definition key
     * @param user
     *            The task can be completed by the given user
     * @param taskService
     *            The Flowable's task service used to request the Flowable engine about task instances.
     * @return The task to complete
     */
    public static Task assertCurrentUserTask(final String processInstanceId, final String taskDefinitionKey,
            final String user, final TaskService taskService) {

        final TaskQuery taskQuery = taskService.createTaskQuery();
        final Task nextTask = taskQuery.processInstanceId(processInstanceId).taskDefinitionKey(taskDefinitionKey)
                .taskCandidateOrAssigned(user).singleResult();
        assertNotNull(nextTask);
        assertEquals(processInstanceId, nextTask.getProcessInstanceId());
        assertEquals(taskDefinitionKey, nextTask.getTaskDefinitionKey());

        return nextTask;
    }

    /**
     * Assertion to check that a user task was completed by the given user.
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param taskDefinitionKey
     *            The process definition key
     * @param user
     *            The task has been completed by the given user
     * @param historyService
     *            The Flowable's history service used to request the Flowable engine about historic process instances.
     */
    public static void assertUserTaskEnded(final String processInstanceId, final String taskDefinitionKey,
            final String user, final HistoryService historyService) {

        final HistoricTaskInstanceQuery taskQuery = historyService.createHistoricTaskInstanceQuery();
        final HistoricTaskInstance nextTask = taskQuery.processInstanceId(processInstanceId)
                .taskDefinitionKey(taskDefinitionKey).singleResult();
        assertNotNull(nextTask);
        assertEquals(user, nextTask.getAssignee());
    }

    /**
     * Assertion to check an intermediate catch message event in wait.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its assignment.
     * @param messageEventName
     *            The message name of the intermediate catch message event
     * @param runtimeService
     *            The Flowable's runtime service used to request the Flowable engine about process instances.
     * @return The {@link Execution} associated to the intermediate catch message event in wait.
     */
    public static Execution assertCurrentIntermediateCatchMessageEvent(final String processInstanceId,
            final String messageEventName, final RuntimeService runtimeService) {

        final ExecutionQuery query = runtimeService.createExecutionQuery().processInstanceId(processInstanceId)
                .messageEventSubscriptionName(messageEventName);
        final Execution execution = query.singleResult();
        assertNotNull(execution, "No pending intermediate catch message event found.");

        return execution;
    }

    /**
     * Assertion to check that a process instance is put as dead letter job.
     * 
     * @param processInstanceId
     *            The identifier of the process instance for which a service task was put put as dead letter job.
     * @param errorMessagePattern
     *            The message pattern of the error putting the job as dead letter job
     * @return The {@link Job} associated to the dead letter job.
     */
    public static Job assertDeadLetterJob(final String processInstanceId, final Pattern errorMessagePattern,
            final ManagementService managementService) {

        final DeadLetterJobQuery query = managementService.createDeadLetterJobQuery()
                .processInstanceId(processInstanceId);
        final Job job = query.singleResult();
        assertNotNull(job, "No pending dead letter job found.");
        final String errorMessage = job.getExceptionMessage().trim();
        assertTrue(errorMessagePattern.matcher(errorMessage).matches(), "Actual error message: " + errorMessage);

        return job;
    }
}
