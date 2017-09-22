/**
 * Copyright (c) 2017 Linagora
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

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.flowable.engine.HistoryService;
import org.flowable.engine.ManagementService;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.TaskService;
import org.flowable.engine.common.api.query.Query;
import org.flowable.engine.history.HistoricActivityInstanceQuery;
import org.flowable.engine.history.HistoricProcessInstanceQuery;
import org.flowable.engine.runtime.DeadLetterJobQuery;
import org.flowable.engine.runtime.ExecutionQuery;
import org.flowable.engine.task.TaskQuery;;

/**
 * Awaiting methods to manage Flowable process events
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class Await {

    /**
     * Wait that a process instance ends successfully. If the waiting time is upper than 60s, an {@link AssertionError}
     * is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the process instance to wait its end.
     */
    public static void waitEndOfProcessInstance(final String processInstanceId, final HistoryService historyService)
            throws InterruptedException {
        waitEndOfProcessInstance(processInstanceId, historyService, 60);
    }

    /**
     * Wait that a process instance ends successfully. If the waiting time is upper than the given one, an
     * {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the process instance to wait its succeeded end.
     * @param duration
     *            Waiting time in seconds before to throw a {@link AssertionError} to fail the processing
     */
    public static void waitEndOfProcessInstance(final String processInstanceId, final HistoryService historyService,
            final int duration) throws InterruptedException {

        final HistoricProcessInstanceQuery histProcInstQuery = historyService.createHistoricProcessInstanceQuery()
                .processInstanceId(processInstanceId).finished();
        if (!Await.waitSingleResult(histProcInstQuery, duration)) {
            throw new AssertionError(String.format("Process instance '%s' not finished in the waiting time ('%ds').",
                    processInstanceId, duration));
        }
    }

    /**
     * Wait that a process instance is put as dead letter job. If the waiting time is upper than 60s, an
     * {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the process instance to wait its placement as dead letter job.
     * @param duration
     *            Waiting time in seconds before to throw a {@link AssertionError} to fail the processing
     */
    public static void waitProcessInstanceAsDeadLetterJob(final String processInstanceId,
            final ManagementService managementService) throws InterruptedException {
        Await.waitProcessInstanceAsDeadLetterJob(processInstanceId, managementService, 60);
    }

    /**
     * Wait that a process instance is put as dead letter job. If the waiting time is upper than the given one, an
     * {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the process instance to wait its placement as dead letter job.
     * @param duration
     *            Waiting time in seconds before to throw a {@link AssertionError} to fail the processing
     */
    public static void waitProcessInstanceAsDeadLetterJob(final String processInstanceId,
            final ManagementService managementService, final int duration) throws InterruptedException {

        final DeadLetterJobQuery deadLetterJobQuery = managementService.createDeadLetterJobQuery()
                .processInstanceId(processInstanceId);
        if (!Await.waitSingleResult(deadLetterJobQuery, duration)) {
            throw new AssertionError(
                    String.format("Process instance '%s' not put as dead letter job in the waiting time ('%ds').",
                            processInstanceId, duration));
        }
    }

    /**
     * Wait that a service task of a process instance ends. If the waiting time is upper than 60s, an
     * {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its end.
     * @param serviceTaskDefinitionKey
     *            The service task identifier in the process definition
     */
    public static void waitEndOfServiceTask(final String processInstanceId, final String serviceTaskDefinitionKey,
            final HistoryService historyService) throws InterruptedException {
        Await.waitEndOfServiceTask(processInstanceId, serviceTaskDefinitionKey, historyService, 60);
    }

    /**
     * Wait that a service task of a process instance ends. If the waiting time is upper than the given one, an
     * {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its end.
     * @param serviceTaskDefinitionKey
     *            The service task identifier in the process definition
     * @param duration
     *            Waiting time in seconds before to throw a {@link AssertionError} to fail the processing
     */
    public static void waitEndOfServiceTask(final String processInstanceId, final String serviceTaskDefinitionKey,
            final HistoryService historyService, final int duration) throws InterruptedException {

        final HistoricActivityInstanceQuery histSvcTaskQuery = historyService.createHistoricActivityInstanceQuery()
                .processInstanceId(processInstanceId).activityId(serviceTaskDefinitionKey).finished();
        if (!Await.waitSingleResult(histSvcTaskQuery, duration)) {
            throw new AssertionError(
                    String.format("Service task '%s' of process instance '%s' not ended in the waiting time ('%ds').",
                            serviceTaskDefinitionKey, processInstanceId, duration));
        }
    }

    /**
     * Wait that a user task of a process instance is assigned to a candidate user. If the waiting time is upper than
     * 60s, an {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its assignment.
     * @param taskDefinitionKey
     *            The user task identifier in the process definition
     * @param candidateUser
     *            The candidate user of the user task
     */
    public static void waitUserTaskAssignment(final String processInstanceId, final String taskDefinitionKey,
            final String candidateUser, final TaskService taskService) throws InterruptedException {
        Await.waitUserTaskAssignment(processInstanceId, taskDefinitionKey, candidateUser, taskService, 60);
    }

    /**
     * Wait that a user task of a process instance is assigned to a candidate user. If the waiting time is upper than
     * the given one, an {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its assignment.
     * @param taskDefinitionKey
     *            The user task identifier in the process definition
     * @param candidateUser
     *            The candidate user of the user task
     * @param duration
     *            Waiting time in seconds before to throw a {@link AssertionError} to fail the processing
     */
    public static void waitUserTaskAssignment(final String processInstanceId, final String taskDefinitionKey,
            final String candidateUser, final TaskService taskService, final int duration) throws InterruptedException {

        final TaskQuery taskQuery = taskService.createTaskQuery().processInstanceId(processInstanceId)
                .taskDefinitionKey(taskDefinitionKey).taskCandidateUser(candidateUser);
        if (!Await.waitSingleResult(taskQuery, duration)) {
            throw new AssertionError(
                    String.format("User task '%s' of process instance '%s' not assigned in the waiting time ('%ds').",
                            taskDefinitionKey, processInstanceId, duration));
        }
    }

    /**
     * Wait that an intermediate catch message event is ready to receive message. If the waiting time is upper than 60s,
     * an {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its assignment.
     * @param messageName
     *            The message name of the intermediate catch message event
     */
    public static void waitIntermediateCatchMessageEvent(final String processInstanceId, final String messageEventName,
            final RuntimeService runtimeService) throws InterruptedException {

        Await.waitIntermediateCatchMessageEvent(processInstanceId, messageEventName, runtimeService, 60);
    }

    /**
     * Wait that an intermediate catch message event is ready to receive message. If the waiting time is upper than the
     * given one, an {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its assignment.
     * @param messageName
     *            The message name of the intermediate catch message event
     * @param duration
     *            Waiting time in seconds before to throw a {@link AssertionError} to fail the processing
     */
    public static void waitIntermediateCatchMessageEvent(final String processInstanceId, final String messageEventName,
            final RuntimeService runtimeService, final int duration) throws InterruptedException {

        final ExecutionQuery executionQuery = runtimeService.createExecutionQuery().processInstanceId(processInstanceId)
                .messageEventSubscriptionName(messageEventName);
        if (!Await.waitSingleResult(executionQuery, duration)) {
            throw new AssertionError(String.format(
                    "Intermediate catch message event of process instance '%s' not ready to receive message '%s' in the waiting time ('%ds').",
                    processInstanceId, messageEventName, duration));
        }
    }

    private static boolean waitSingleResult(final Query query, final int duration) throws InterruptedException {
        final CountDownLatch lock = new CountDownLatch(1);
        final Thread waitingThread = new Thread(new Runnable() {
            @Override
            public void run() {
                boolean run = true;
                try {
                    while (run) {
                        Thread.sleep(250);
                        // Caution: service tasks are stored in activity historic, not in task historic.
                        if (query.singleResult() != null) {
                            // the process instance is finished
                            run = false;
                            lock.countDown();
                        }
                    }
                } catch (final InterruptedException e) {
                    e.printStackTrace();
                }
            }
        });
        waitingThread.start();
        return lock.await(duration, TimeUnit.SECONDS);
    }

}
