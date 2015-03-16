/**
 * Copyright (c) 2014-2015 Linagora
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
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.xml.namespace.QName;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.RuleChain;
import org.junit.rules.TestRule;
import org.ow2.petals.activitibpmn.exception.IncoherentProcessDefinitionDeclarationException;
import org.ow2.petals.activitibpmn.exception.InvalidVersionDeclaredException;
import org.ow2.petals.activitibpmn.exception.NoProcessDefinitionDeclarationException;
import org.ow2.petals.activitibpmn.exception.UnexistingProcessFileException;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration.ServiceType;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTest;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;
import org.ow2.petals.jbi.descriptor.JBIDescriptorException;
import org.ow2.petals.junit.rules.log.handler.InMemoryLogHandler;

/**
 * Unit tests of {@link ActivitiSuManager}
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ActivitiSuManagerTest extends AbstractTest {

    private static final QName SERVICE_NAME = new QName("namespace", "a-service");

    private static final QName INTERFACE_NAME = new QName("namespace", "an-interface");

    private static final String ENDPOINT_NAME = "an-endpoint-name";

    private static final String A_PROCESS_DEFINITION_NAME = "orderProcess.bpmn20.xml";

    private static final String ANOTHER_PROCESS_DEFINITION_NAME = "vacationRequest.bpmn20.xml";

    private static final URL A_PROCESS_DEFINITION_URL = Thread.currentThread().getContextClassLoader()
            .getResource(A_PROCESS_DEFINITION_NAME);

    private static final InMemoryLogHandler IN_MEMORY_LOG_HANDLER = new InMemoryLogHandler();

    private static final ComponentUnderTest COMPONENT_UNDER_TEST = new ComponentUnderTest()
            .addLogHandler(IN_MEMORY_LOG_HANDLER.getHandler());

    @ClassRule
    public static final TestRule chain = RuleChain.outerRule(IN_MEMORY_LOG_HANDLER).around(COMPONENT_UNDER_TEST);

    @BeforeClass
    public static void checkResources() {
        assertNotNull("Process definition file not found: " + A_PROCESS_DEFINITION_NAME, A_PROCESS_DEFINITION_URL);
        assertNotNull("Process definition file not found: " + ANOTHER_PROCESS_DEFINITION_NAME,
                ANOTHER_PROCESS_DEFINITION_NAME);
    }

    @After
    public void undeployAllServices() {
        COMPONENT_UNDER_TEST.undeployAllServices();
    }

    @After
    public void cleanLogTraces() {
        IN_MEMORY_LOG_HANDLER.clear();
    }

    /**
     * <p>
     * Try do deploy a SU containing no process declaration
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>an error occurs because no process declaration is included into the JBI descriptor</li>
     * <li>the error is logged as ERROR</li>
     * </ul>
     * </p>
     */
    @Test
    public void deploy_NoProcessDefinition() throws SecurityException, IllegalArgumentException,
            JBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final String suName = "no-process-definition";
        try {
            COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(suName, INTERFACE_NAME,
                            SERVICE_NAME, ENDPOINT_NAME, ServiceType.PROVIDE, null);

                    return serviceConfiguration;
                }
            });
        } catch (final DeploymentException e) {
            // Expected exception
            assertTrue("The deployment succeeds: " + e.getMessage(),
                    e.getMessage().contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
            assertTrue("Unexpected error: " + e.getMessage(),
                    e.getMessage().contains(NoProcessDefinitionDeclarationException.MESSAGE));

            final List<LogRecord> errors = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.SEVERE);
            assertEquals(1, errors.size());
            final LogRecord error = errors.get(0);
            assertTrue(error.getThrown() instanceof NoProcessDefinitionDeclarationException);
        }
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing one process declaration: the process file is set without version.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>an error occurs because the field 'version' is missing into the JBI descriptor</li>
     * <li>the error is logged as ERROR</li>
     * </ul>
     * </p>
     */
    @Test
    public void deploy_OneProcessDefinition_MissingVersion() throws SecurityException, IllegalArgumentException,
            JBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final String suName = "one-process-definition-missing-version";
        try {
            COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(suName, INTERFACE_NAME,
                            SERVICE_NAME, ENDPOINT_NAME, ServiceType.PROVIDE, null);

                    serviceConfiguration.addResource(A_PROCESS_DEFINITION_URL);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE).toString(), A_PROCESS_DEFINITION_NAME);

                    return serviceConfiguration;
                }
            });
        } catch (final DeploymentException e) {
            // Expected exception
            assertTrue("The deployment succeeds: " + e.getMessage(),
                    e.getMessage().contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
            assertTrue(
                    "Unexpected error: " + e.getMessage(),
                    e.getMessage().contains(
                            String.format(IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN,
                                    A_PROCESS_DEFINITION_NAME, null)));

            final List<LogRecord> errors = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.SEVERE);
            assertEquals(1, errors.size());
            final LogRecord error = errors.get(0);
            assertTrue(error.getThrown() instanceof IncoherentProcessDefinitionDeclarationException);
        }
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing one process declaration: the process file is set with an empty version.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>an error occurs because the field 'version' is missing into the JBI descriptor</li>
     * <li>the error is logged as ERROR</li>
     * </ul>
     * </p>
     */
    @Test
    public void deploy_OneProcessDefinition_EmptyVersion() throws SecurityException, IllegalArgumentException,
            JBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final String suName = "one-process-definition-empty-version";
        try {
            COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(suName, INTERFACE_NAME,
                            SERVICE_NAME, ENDPOINT_NAME, ServiceType.PROVIDE, null);

                    serviceConfiguration.addResource(A_PROCESS_DEFINITION_URL);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE).toString(), A_PROCESS_DEFINITION_NAME);
                    serviceConfiguration.setParameter(
                            "{http://petals.ow2.org/components/petals-se-activitibpmn/version-1.0}version", "");

                    return serviceConfiguration;
                }
            });
        } catch (final DeploymentException e) {
            // Expected exception
            assertTrue("The deployment succeeds: " + e.getMessage(),
                    e.getMessage().contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
            assertTrue(
                    "Unexpected error: " + e.getMessage(),
                    e.getMessage().contains(
                            String.format(IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN,
                                    A_PROCESS_DEFINITION_NAME, "")));

            final List<LogRecord> errors = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.SEVERE);
            assertEquals(1, errors.size());
            final LogRecord error = errors.get(0);
            assertTrue(error.getThrown() instanceof IncoherentProcessDefinitionDeclarationException);
        }
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing one process declaration: the process file is set with an invalid version.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>an error occurs because the field 'version' is set to an invalid value into the JBI descriptor</li>
     * <li>the error is logged as ERROR</li>
     * </ul>
     * </p>
     */
    @Test
    public void deploy_OneProcessDefinition_InvalidVersion() throws SecurityException, IllegalArgumentException,
            JBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final String suName = "one-process-definition-invalid-version";
        final String version = "invalid";
        try {
            COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(suName, INTERFACE_NAME,
                            SERVICE_NAME, ENDPOINT_NAME, ServiceType.PROVIDE, null);

                    serviceConfiguration.addResource(A_PROCESS_DEFINITION_URL);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE).toString(), A_PROCESS_DEFINITION_NAME);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION).toString(), version);

                    return serviceConfiguration;
                }
            });
        } catch (final DeploymentException e) {
            // Expected exception
            assertTrue("The deployment succeeds: " + e.getMessage(),
                    e.getMessage().contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
            assertTrue(
                    "Unexpected error: " + e.getMessage(),
                    e.getMessage().contains(
                            String.format(InvalidVersionDeclaredException.MESSAGE_PATTERN, A_PROCESS_DEFINITION_NAME,
                                    version)));

            final List<LogRecord> errors = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.SEVERE);
            assertEquals(1, errors.size());
            final LogRecord error = errors.get(0);
            assertTrue(error.getThrown() instanceof InvalidVersionDeclaredException);
        }
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing one process declaration: the process version is set without its file.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>an error occurs because the field 'process-file' is missing into the JBI descriptor</li>
     * <li>the error is logged as ERROR</li>
     * </ul>
     * </p>
     */
    @Test
    public void deploy_OneProcessDefinition_MissingProcessFile() throws SecurityException, IllegalArgumentException,
            JBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final String suName = "one-process-definition-missing-processfile";
        final String version = "1";
        try {
            COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(suName, INTERFACE_NAME,
                            SERVICE_NAME, ENDPOINT_NAME, ServiceType.PROVIDE, null);

                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION).toString(), version);

                    return serviceConfiguration;
                }
            });
        } catch (final DeploymentException e) {
            // Expected exception
            assertTrue("The deployment succeeds: " + e.getMessage(),
                    e.getMessage().contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
            assertTrue(
                    "Unexpected error: " + e.getMessage(),
                    e.getMessage().contains(
                            String.format(IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN, null,
                                    version)));

            final List<LogRecord> errors = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.SEVERE);
            assertEquals(1, errors.size());
            final LogRecord error = errors.get(0);
            assertTrue(error.getThrown() instanceof IncoherentProcessDefinitionDeclarationException);
        }
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing one process declaration: the process version is set and the process file
     * is set to an empty value.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>an error occurs because the field 'process-file' is empty into the JBI descriptor</li>
     * <li>the error is logged as ERROR</li>
     * </ul>
     * </p>
     */
    @Test
    public void deploy_OneProcessDefinition_EmptyProcessFile() throws SecurityException, IllegalArgumentException,
            JBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final String suName = "one-process-definition-empty-processfile";
        final String version = "1";
        try {
            COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(suName, INTERFACE_NAME,
                            SERVICE_NAME, ENDPOINT_NAME, ServiceType.PROVIDE, null);

                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE).toString(), "");
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION).toString(), version);

                    return serviceConfiguration;
                }
            });
        } catch (final DeploymentException e) {
            // Expected exception
            assertTrue("The deployment succeeds: " + e.getMessage(),
                    e.getMessage().contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
            assertTrue(
                    "Unexpected error: " + e.getMessage(),
                    e.getMessage()
                            .contains(
                                    String.format(IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN, "",
                                            version)));

            final List<LogRecord> errors = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.SEVERE);
            assertEquals(1, errors.size());
            final LogRecord error = errors.get(0);
            assertTrue(error.getThrown() instanceof IncoherentProcessDefinitionDeclarationException);
        }
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing one process declaration: the process version is set and the process file
     * is set to an inexisting file.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>an error occurs because the field 'process-file' does not exist</li>
     * <li>the error is logged as ERROR</li>
     * </ul>
     * </p>
     */
    @Test
    public void deploy_OneProcessDefinition_UnknownProcessFile() throws SecurityException, IllegalArgumentException,
            JBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final String suName = "one-process-definition-unknown-process-file";
        final String version = "1";
        final String processFile = "unexisting-process-file.bpmn";
        try {
            COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(suName, INTERFACE_NAME,
                            SERVICE_NAME, ENDPOINT_NAME, ServiceType.PROVIDE, null);

                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE).toString(), processFile);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION).toString(), version);

                    return serviceConfiguration;
                }
            });
        } catch (final DeploymentException e) {
            // Expected exception
            assertTrue("The deployment succeeds: " + e.getMessage(),
                    e.getMessage().contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
            assertTrue(
                    "Unexpected error: " + e.getMessage(),
                    e.getMessage().contains(
                            String.format(
                                    UnexistingProcessFileException.MESSAGE_PATTERN,
                                    new File(COMPONENT_UNDER_TEST
                                            .getServiceConfigurationInstallDir(COMPONENT_UNDER_TEST
                                                    .getServiceConfiguration(suName)), processFile).getAbsolutePath())));

            final List<LogRecord> errors = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.SEVERE);
            assertEquals(1, errors.size());
            final LogRecord error = errors.get(0);
            assertTrue(error.getThrown() instanceof UnexistingProcessFileException);
        }
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing multiple process declarations where:
     * <ul>
     * <li>a first process declaration is valid,</li>
     * <li>the 2nd process declaration: the process file is set without version.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>As version occurs once, an error about a missing process file occurs because the CDK has read multiple
     * process files as '<code>process_file&lt;i&gt;</code>' but the unique version as '<code>version</code>'</li>
     * <li>the error is logged as ERROR</li>
     * </ul>
     * </p>
     */
    @Test
    public void deploy_MultipleProcessDefinition_MissingVersion() throws SecurityException, IllegalArgumentException,
            JBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final String suName = "multiple-process-definition-missing-version";
        final String version = "1";
        try {
            COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(suName, INTERFACE_NAME,
                            SERVICE_NAME, ENDPOINT_NAME, ServiceType.PROVIDE, null);

                    serviceConfiguration.addResource(A_PROCESS_DEFINITION_URL);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE + "1").toString(), A_PROCESS_DEFINITION_NAME);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION + "1").toString(), version);

                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE + "2").toString(), ANOTHER_PROCESS_DEFINITION_NAME);

                    return serviceConfiguration;
                }
            });
        } catch (final DeploymentException e) {
            // Expected exception
            assertTrue("The deployment succeeds: " + e.getMessage(),
                    e.getMessage().contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
            assertTrue(
                    "Unexpected error: " + e.getMessage(),
                    e.getMessage().contains(
                            String.format(IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN,
                                    ANOTHER_PROCESS_DEFINITION_NAME, null)));

            final List<LogRecord> errors = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.SEVERE);
            assertEquals(1, errors.size());
            final LogRecord error = errors.get(0);
            assertTrue(error.getThrown() instanceof IncoherentProcessDefinitionDeclarationException);
        }
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing multiple process declarations where:
     * <ul>
     * <li>a first process declaration is valid,</li>
     * <li>the 2nd process declaration: the process file is set with an empty version.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>an error occurs because the 2nd field 'version' is empty into the JBI descriptor</li>
     * <li>the error is logged as ERROR</li>
     * </ul>
     * </p>
     */
    @Test
    public void deploy_MultipleProcessDefinition_EmptyVersion() throws SecurityException, IllegalArgumentException,
            JBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final String suName = "multiple-process-definition-empty-version";
        final String version = "1";
        try {
            COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(suName, INTERFACE_NAME,
                            SERVICE_NAME, ENDPOINT_NAME, ServiceType.PROVIDE, null);

                    serviceConfiguration.addResource(A_PROCESS_DEFINITION_URL);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE + "1").toString(), A_PROCESS_DEFINITION_NAME);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION + "1").toString(), version);

                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE + "2").toString(), ANOTHER_PROCESS_DEFINITION_NAME);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION + "2").toString(), "");

                    return serviceConfiguration;
                }
            });
        } catch (final DeploymentException e) {
            // Expected exception
            assertTrue("The deployment succeeds: " + e.getMessage(),
                    e.getMessage().contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
            assertTrue(
                    "Unexpected error: " + e.getMessage(),
                    e.getMessage().contains(
                            String.format(IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN,
                                    ANOTHER_PROCESS_DEFINITION_NAME, "")));

            final List<LogRecord> errors = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.SEVERE);
            assertEquals(1, errors.size());
            final LogRecord error = errors.get(0);
            assertTrue(error.getThrown() instanceof IncoherentProcessDefinitionDeclarationException);
        }
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing multiple process declarations where:
     * <ul>
     * <li>a first process declaration is valid,</li>
     * <li>the 2nd process declaration: the process file is set with an invalid version.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>an error occurs because the 2nd field 'version' is invalid into the JBI descriptor</li>
     * <li>the error is logged as ERROR</li>
     * </ul>
     * </p>
     */
    @Test
    public void deploy_MultipleProcessDefinition_InvalidVersion() throws SecurityException, IllegalArgumentException,
            JBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final String suName = "multiple-process-definition-invalid-version";
        final String version1 = "1";
        final String version2 = "invalid";
        try {
            COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(suName, INTERFACE_NAME,
                            SERVICE_NAME, ENDPOINT_NAME, ServiceType.PROVIDE, null);

                    serviceConfiguration.addResource(A_PROCESS_DEFINITION_URL);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE + "1").toString(), A_PROCESS_DEFINITION_NAME);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION + "1").toString(), version1);

                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE + "2").toString(), ANOTHER_PROCESS_DEFINITION_NAME);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION + "2").toString(), version2);

                    return serviceConfiguration;
                }
            });
        } catch (final DeploymentException e) {
            // Expected exception
            assertTrue("The deployment succeeds: " + e.getMessage(),
                    e.getMessage().contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
            assertTrue(
                    "Unexpected error: " + e.getMessage(),
                    e.getMessage().contains(
                            String.format(InvalidVersionDeclaredException.MESSAGE_PATTERN,
                                    ANOTHER_PROCESS_DEFINITION_NAME, version2)));

            final List<LogRecord> errors = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.SEVERE);
            assertEquals(1, errors.size());
            final LogRecord error = errors.get(0);
            assertTrue(error.getThrown() instanceof InvalidVersionDeclaredException);
        }
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing multiple process declarations where:
     * <ul>
     * <li>a first process declaration is valid,</li>
     * <li>the 2nd process declaration: the process version is set without file.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>As process file occurs once, an error about a missing version occurs because the CDK has read multiple
     * process files as '<code>version&lt;i&gt;</code>' but the unique version as '<code>process_file</code>'</li>
     * <li>the error is logged as ERROR</li>
     * </ul>
     * </p>
     */
    @Test
    public void deploy_MultipleProcessDefinition_MissingProcessFile() throws SecurityException,
            IllegalArgumentException, JBIDescriptorException, NoSuchFieldException, IllegalAccessException,
            IOException, JBIException, URISyntaxException {

        final String suName = "multiple-process-definition-missing-process-file";
        final String version1 = "1";
        final String version2 = "2";
        try {
            COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(suName, INTERFACE_NAME,
                            SERVICE_NAME, ENDPOINT_NAME, ServiceType.PROVIDE, null);

                    serviceConfiguration.addResource(A_PROCESS_DEFINITION_URL);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE + "1").toString(), A_PROCESS_DEFINITION_NAME);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION + "1").toString(), version1);

                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION + "2").toString(), version2);

                    return serviceConfiguration;
                }
            });
        } catch (final DeploymentException e) {
            // Expected exception
            assertTrue("The deployment succeeds: " + e.getMessage(),
                    e.getMessage().contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
            assertTrue(
                    "Unexpected error: " + e.getMessage(),
                    e.getMessage().contains(
                            String.format(IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN, null,
                                    version2)));

            final List<LogRecord> errors = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.SEVERE);
            assertEquals(1, errors.size());
            final LogRecord error = errors.get(0);
            assertTrue(error.getThrown() instanceof IncoherentProcessDefinitionDeclarationException);
        }
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing multiple process declarations where:
     * <ul>
     * <li>a first process declaration is valid,</li>
     * <li>the 2nd process declaration: the process version is set with an empty file.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>an error occurs because the 2nd field 'version' is empty into the JBI descriptor</li>
     * <li>the error is logged as ERROR</li>
     * </ul>
     * </p>
     */
    @Test
    public void deploy_MultipleProcessDefinition_EmptyProcessFile() throws SecurityException, IllegalArgumentException,
            JBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final String suName = "multiple-process-definition-missing-process-file";
        final String version1 = "1";
        final String version2 = "2";
        try {
            COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(suName, INTERFACE_NAME,
                            SERVICE_NAME, ENDPOINT_NAME, ServiceType.PROVIDE, null);

                    serviceConfiguration.addResource(A_PROCESS_DEFINITION_URL);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE + "1").toString(), A_PROCESS_DEFINITION_NAME);
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION + "1").toString(), version1);

                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.PROCESS_FILE + "2").toString(), "");
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU,
                            ActivitiSEConstants.VERSION + "2").toString(), version2);

                    return serviceConfiguration;
                }
            });
        } catch (final DeploymentException e) {
            // Expected exception
            assertTrue("The deployment succeeds: " + e.getMessage(),
                    e.getMessage().contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
            assertTrue(
                    "Unexpected error: " + e.getMessage(),
                    e.getMessage()
                            .contains(
                                    String.format(IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN, "",
                                            version2)));

            final List<LogRecord> errors = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.SEVERE);
            assertEquals(1, errors.size());
            final LogRecord error = errors.get(0);
            assertTrue(error.getThrown() instanceof IncoherentProcessDefinitionDeclarationException);
        }
    }

    /**
     * <p>
     * Try do deploy an valid SU containing multiple process declarations.
     * </p>
     * <p>
     * Expected results: No error occurs
     * </p>
     */
    @Test
    public void deploy_MultipleProcessDefinition() throws SecurityException, IllegalArgumentException,
            JBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final String suName = "MultipleProcessDefintion";
        COMPONENT_UNDER_TEST.deployService(suName, new ServiceConfigurationFactory() {
            @Override
            public ServiceConfiguration create() {

                final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource("su/valid/vacationRequest.wsdl");
                assertNotNull("WSDl not found", wsdlUrl);
                final ServiceConfiguration serviceConfiguration = new ServiceConfiguration(AbstractComponentTest.class
                        .getSimpleName(), AbstractComponentTest.VACATION_INTERFACE,
                        AbstractComponentTest.VACATION_SERVICE, AbstractComponentTest.VACATION_ENDPOINT,
                        ServiceType.PROVIDE, wsdlUrl);

                final URL demanderCongesResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource("su/valid/demanderCongesResponse.xsl");
                assertNotNull("Output XSL 'demanderCongesResponse.xsl' not found", demanderCongesResponseXslUrl);
                serviceConfiguration.addResource(demanderCongesResponseXslUrl);

                final URL validerDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource("su/valid/validerDemandeResponse.xsl");
                assertNotNull("Output XSL 'validerDemandeResponse.xsl' not found", validerDemandeResponseXslUrl);
                serviceConfiguration.addResource(validerDemandeResponseXslUrl);

                final URL ajusterDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource("su/valid/ajusterDemandeResponse.xsl");
                assertNotNull("Output XSL 'ajusterDemandeResponse.xsl' not found", ajusterDemandeResponseXslUrl);
                serviceConfiguration.addResource(ajusterDemandeResponseXslUrl);

                final URL numeroDemandeInconnuXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource("su/valid/numeroDemandeInconnu.xsl");
                assertNotNull("Output XSL 'numeroDemandeInconnu.xsl' not found", numeroDemandeInconnuXslUrl);
                serviceConfiguration.addResource(numeroDemandeInconnuXslUrl);

                final URL demandeDejaValideeXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource("su/valid/demandeDejaValidee.xsl");
                assertNotNull("Output XSL 'demandeDejaValidee.xsl' not found", demandeDejaValideeXslUrl);
                serviceConfiguration.addResource(demandeDejaValideeXslUrl);

                final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                        .getResource("su/valid/vacationRequest.bpmn20.xml");
                assertNotNull("BPMN file not found", bpmnUrl);
                serviceConfiguration.addResource(bpmnUrl);

                // First process definition
                serviceConfiguration.setParameter(
                        "{http://petals.ow2.org/components/petals-se-activitibpmn/version-1.0}process_file1",
                        "vacationRequest.bpmn20.xml");
                serviceConfiguration.setParameter(
                        "{http://petals.ow2.org/components/petals-se-activitibpmn/version-1.0}version1", "1");

                // 2nd process definition
                serviceConfiguration.setParameter(
                        "{http://petals.ow2.org/components/petals-se-activitibpmn/version-1.0}process_file2",
                        "vacationRequest.bpmn20.xml");
                serviceConfiguration.setParameter(
                        "{http://petals.ow2.org/components/petals-se-activitibpmn/version-1.0}version2", "1");

                return serviceConfiguration;
            }
        });

        assertNotNull(COMPONENT_UNDER_TEST.getServiceConfiguration(suName));
    }
}