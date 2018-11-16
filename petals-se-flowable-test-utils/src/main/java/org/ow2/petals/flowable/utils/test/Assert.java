/**
 * Copyright (c) 2017-2018 Linagora
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

import org.flowable.engine.HistoryService;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.TaskService;
import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.history.HistoricProcessInstanceQuery;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.runtime.ProcessInstanceQuery;
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
public class Assert extends org.junit.Assert {

    /**
     * Assertion to check that a process instance is running. The process instance is not finished.
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param processDefinitionKey
     *            The process definition key
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
     * @return The task to complete
     */
    public static Task assertCurrentUserTask(final String processInstanceId, final String taskDefinitionKey,
            final String user, final TaskService taskService) {

        final TaskQuery taskQuery = taskService.createTaskQuery();
        final Task nextTask = taskQuery.processInstanceId(processInstanceId).taskCandidateOrAssigned(user)
                .singleResult();
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
     */
    public static void assertUserTaskEnded(final String processInstanceId, final String taskDefinitionKey,
            final String user, final HistoryService historyService) {

        final HistoricTaskInstanceQuery taskQuery = historyService.createHistoricTaskInstanceQuery();
        final HistoricTaskInstance nextTask = taskQuery.processInstanceId(processInstanceId)
                .taskDefinitionKey(taskDefinitionKey).singleResult();
        assertNotNull(nextTask);
        assertEquals(user, nextTask.getAssignee());
    }

}
