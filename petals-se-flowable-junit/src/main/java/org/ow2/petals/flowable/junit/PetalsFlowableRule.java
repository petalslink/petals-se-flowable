/**
 * Copyright (c) 2017-2019 Linagora
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
package org.ow2.petals.flowable.junit;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.flowable.engine.common.api.delegate.event.FlowableEngineEventType;
import org.flowable.engine.runtime.Execution;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.test.FlowableRule;
import org.flowable.task.api.Task;
import org.junit.runner.Description;
import org.ow2.petals.flowable.utils.test.Assert;
import org.ow2.petals.flowable.utils.test.Await;

public class PetalsFlowableRule extends FlowableRule {

    private static final Logger LOG = Logger.getLogger(PetalsFlowableRule.class.getName());

    private final Map<String, String> placeholders;

    private CallActivityStartedEventListener callActivityStartEventListener = null;

    private ProcessInstanceStartedEventListener processInstanceStartedEventListener = null;

    public PetalsFlowableRule() {
        super();
        this.placeholders = new HashMap<>();
    }

    public PetalsFlowableRule(final Map<String, String> placeholders) {
        super();
        this.placeholders = placeholders;
    }

    @Override
    public void configureProcessEngine() {
        super.configureProcessEngine();

        this.callActivityStartEventListener = new CallActivityStartedEventListener(
                this.getRuntimeService(), this.placeholders, LOG);
        this.getRuntimeService().addEventListener(this.callActivityStartEventListener,
                FlowableEngineEventType.PROCESS_STARTED);

        this.processInstanceStartedEventListener = new ProcessInstanceStartedEventListener(this.getRuntimeService(),
                this.placeholders, LOG);
        this.getRuntimeService().addEventListener(this.processInstanceStartedEventListener,
                FlowableEngineEventType.PROCESS_STARTED);
    }

    @Override
    public void finished(final Description description) {
        if (this.callActivityStartEventListener != null) {
            this.getRuntimeService().removeEventListener(this.callActivityStartEventListener);
            this.callActivityStartEventListener = null;
        }

        if (this.processInstanceStartedEventListener != null) {
            this.getRuntimeService().removeEventListener(this.processInstanceStartedEventListener);
            this.processInstanceStartedEventListener = null;
        }

        super.finished(description);
    }

    /**
     * Assertion to check that a process instance is running. The process instance is not finished.
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param processDefinitionKey
     *            The process definition key
     */
    public void assertProcessInstancePending(final String processInstanceId, final String processDefinitionKey) {

        Assert.assertProcessInstancePending(processInstanceId, processDefinitionKey, this.getRuntimeService());
    }

    /**
     * Assertion to check that a process instance is finished.
     * 
     * @param processInstanceId
     *            The process instance identifier
     */
    public void assertProcessInstanceFinished(final String processInstanceId) {

        Assert.assertProcessInstanceFinished(processInstanceId, this.getHistoryService());
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
    public Task assertCurrentUserTask(final String processInstanceId, final String taskDefinitionKey,
            final String user) {

        return Assert.assertCurrentUserTask(processInstanceId, taskDefinitionKey, user, this.getTaskService());
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
    public void assertUserTaskEnded(final String processInstanceId, final String taskDefinitionKey, final String user) {

        Assert.assertUserTaskEnded(processInstanceId, taskDefinitionKey, user, this.getHistoryService());
    }

    /**
     * Assertion to check an intermediate catch message event in wait.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its assignment.
     * @param messageEventName
     *            The message name of the intermediate catch message event
     * @return The {@link Execution} associated to the intermediate catch message event in wait.
     */
    public Execution assertCurrentIntermediateCatchMessageEvent(final String processInstanceId,
            final String messageEventName) {

        return Assert.assertCurrentIntermediateCatchMessageEvent(processInstanceId, messageEventName,
                this.getRuntimeService());
    }

    /**
     * Wait that a process instance ends successfully. If the waiting time is upper than 60s, an {@link AssertionError}
     * is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the process instance to wait its succeeded end.
     */
    public void waitEndOfProcessInstance(final String processInstanceId) throws InterruptedException {
        Await.waitEndOfProcessInstance(processInstanceId, this.getHistoryService(), 60);
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
    public void waitEndOfProcessInstance(final String processInstanceId, final int duration)
            throws InterruptedException {
        Await.waitEndOfProcessInstance(processInstanceId, this.getHistoryService(), duration);
    }

    /**
     * Wait that a sub-process instance is instantiated. If the waiting time is upper than 60s, an
     * {@link AssertionError} is thrown.
     * 
     * @param superProcessInstanceId
     *            The identifier of the process instance that launches the expected sub-process.
     * @param subProcessDefinitionKey
     *            The definition key of the expected sub-process instance.
     */
    public ProcessInstance waitEndOfProcessInstance(final String superProcessInstanceId,
            final String subProcessDefinitionKey) throws InterruptedException {
        return Await.waitSubProcessus(superProcessInstanceId, subProcessDefinitionKey, this.getRuntimeService(), 60);
    }

    /**
     * Wait that a sub-process instance is instantiated. If the waiting time is upper than the given one, an
     * {@link AssertionError} is thrown.
     * 
     * @param superProcessInstanceId
     *            The identifier of the process instance that launches the expected sub-process.
     * @param subProcessDefinitionKey
     *            The definition key of the expected sub-process instance.
     * @param duration
     *            Waiting time in seconds before to throw a {@link AssertionError} to fail the processing
     */
    public ProcessInstance waitSubProcessus(final String superProcessInstanceId, final String subProcessDefinitionKey,
            final int duration) throws InterruptedException {

        return Await.waitSubProcessus(superProcessInstanceId, subProcessDefinitionKey, this.getRuntimeService(),
                duration);
    }

    /**
     * Wait that a process instance is put as dead letter job. If the waiting time is upper than 60s, an
     * {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the process instance to wait its placement as dead letter job.
     */
    public void waitProcessInstanceAsDeadLetterJob(final String processInstanceId) throws InterruptedException {
        Await.waitProcessInstanceAsDeadLetterJob(processInstanceId, this.getManagementService(), 60);
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
    public void waitProcessInstanceAsDeadLetterJob(final String processInstanceId, final int duration)
            throws InterruptedException {
        Await.waitProcessInstanceAsDeadLetterJob(processInstanceId, this.getManagementService(), duration);
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
    public void waitEndOfServiceTask(final String processInstanceId, final String serviceTaskDefinitionKey)
            throws InterruptedException {
        Await.waitEndOfServiceTask(processInstanceId, serviceTaskDefinitionKey, this.getHistoryService(), 60);
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
    public void waitEndOfServiceTask(final String processInstanceId, final String serviceTaskDefinitionKey,
            final int duration) throws InterruptedException {

        Await.waitEndOfServiceTask(processInstanceId, serviceTaskDefinitionKey, this.getHistoryService(), duration);
    }

    /**
     * Wait that a service task of a process instance ends. If the waiting time is upper than 60s, an
     * {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its end.
     * @param taskDefinitionKey
     *            The service task identifier (definition key) in the process definition
     */
    public void waitUserTaskAssignment(final String processInstanceId, final String taskDefinitionKey,
            final String candidateUser) throws InterruptedException {
        Await.waitUserTaskAssignment(processInstanceId, taskDefinitionKey, candidateUser, this.getTaskService(), 60);
    }

    /**
     * Wait that a service task of a process instance ends. If the waiting time is upper than the given one, an
     * {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its end.
     * @param taskDefinitionKey
     *            The task identifier (definition key) in the process definition
     * @param duration
     *            Waiting time in seconds before to throw a {@link AssertionError} to fail the processing
     */
    public void waitUserTaskAssignment(final String processInstanceId, final String taskDefinitionKey,
            final String candidateUser, final int duration) throws InterruptedException {

        Await.waitUserTaskAssignment(processInstanceId, taskDefinitionKey, candidateUser, this.getTaskService(),
                duration);
    }

    /**
     * Wait that an intermediate catch message event is ready to receive message. If the waiting time is upper than 60s,
     * an {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its assignment.
     * @param messageEventName
     *            The message name of the intermediate catch message event
     */
    public void waitIntermediateCatchMessageEvent(final String processInstanceId, final String messageEventName)
            throws InterruptedException {

        Await.waitIntermediateCatchMessageEvent(processInstanceId, messageEventName, this.getRuntimeService(), 60);
    }

    /**
     * Wait that an intermediate catch message event is ready to receive message. If the waiting time is upper than the
     * given one, an {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its assignment.
     * @param messageEventName
     *            The message name of the intermediate catch message event
     * @param duration
     *            Waiting time in seconds before to throw a {@link AssertionError} to fail the processing
     */
    public void waitIntermediateCatchMessageEvent(final String processInstanceId, final String messageEventName,
            final int duration) throws InterruptedException {

        Await.waitIntermediateCatchMessageEvent(processInstanceId, messageEventName, this.getRuntimeService(),
                duration);
    }

    /**
     * Signal an intermediate catch message event.
     * 
     * @param processInstanceId
     *            The identifier of the process instance waiting the intermediate catch message event
     * @param messageEventName
     *            The message name of the intermediate catch message event
     */
    public void signalIntermediateCatchMessageEvent(final String processInstanceId, final String messageEventName) {

        this.signalIntermediateCatchMessageEvent(processInstanceId, messageEventName, null);

    }

    /**
     * <p>
     * Signal an intermediate catch message event, providing variables to set on process instance.
     * </p>
     * <p>
     * This method will do a search query to retrieve the execution associated to the intermediate catch message event.
     * To avoid to do a new search query, use {@link #signalIntermediateCatchMessageEvent(Execution, String)}.
     * </p>
     * 
     * @param processInstanceId
     *            The identifier of the process instance waiting the intermediate catch message event
     * @param messageEventName
     *            The message name of the intermediate catch message event
     * @param variables
     *            Process variables to set signaling the intermediate catch message event
     */
    public void signalIntermediateCatchMessageEvent(final String processInstanceId, final String messageEventName,
            final Map<String, Object> variables) {

        final Execution execution = this.assertCurrentIntermediateCatchMessageEvent(processInstanceId,
                messageEventName);

        if (variables == null) {
            this.getRuntimeService().messageEventReceived(messageEventName, execution.getId());
        } else {
            this.getRuntimeService().messageEventReceived(messageEventName, execution.getId(), variables);
        }

    }

    /**
     * Signal an intermediate catch message event.
     * 
     * @param execution
     *            The {@link Execution} associated to the intermediate catch message event in wait.
     * @param messageEventName
     *            The message name of the intermediate catch message event
     */
    public void signalIntermediateCatchMessageEvent(final Execution execution, final String messageEventName) {

        this.signalIntermediateCatchMessageEvent(execution, messageEventName, null);

    }

    /**
     * Signal an intermediate catch message event, providing variables to set on process instance.
     * 
     * @param execution
     *            The {@link Execution} associated to the intermediate catch message event in wait.
     * @param messageEventName
     *            The message name of the intermediate catch message event
     * @param variables
     *            Process variables to set signaling the intermediate catch message event
     */
    public void signalIntermediateCatchMessageEvent(final Execution execution, final String messageEventName,
            final Map<String, Object> variables) {

        if (variables == null) {
            this.getRuntimeService().messageEventReceived(messageEventName, execution.getId());
        } else {
            this.getRuntimeService().messageEventReceived(messageEventName, execution.getId(), variables);
        }

    }

}
