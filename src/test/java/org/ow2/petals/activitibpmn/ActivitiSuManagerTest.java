/**
 * Copyright (c) 2015 Linagora
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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.activiti.engine.ProcessEngine;
import org.activiti.engine.ProcessEngineConfiguration;
import org.apache.commons.io.IOUtils;
import org.easymock.EasyMock;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.ow2.petals.activitibpmn.exception.IncoherentProcessDefinitionDeclarationException;
import org.ow2.petals.activitibpmn.exception.InvalidVersionDeclaredException;
import org.ow2.petals.activitibpmn.exception.NoProcessDefinitionDeclarationException;
import org.ow2.petals.activitibpmn.exception.UnexistingProcessFileException;
import org.ow2.petals.component.framework.AbstractComponent;
import org.ow2.petals.component.framework.api.Interceptor;
import org.ow2.petals.component.framework.jbidescriptor.CDKJBIDescriptorException;
import org.ow2.petals.component.framework.jbidescriptor.JBIDescriptorBuilder;
import org.ow2.petals.component.framework.jbidescriptor.generated.Jbi;
import org.ow2.petals.component.framework.jbidescriptor.generated.Provides;
import org.ow2.petals.component.framework.jbidescriptor.generated.Services;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;

import com.ebmwebsourcing.easycommons.xml.DocumentBuilders;

/**
 * Unit tests of {@link ActivitiSuManager}
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ActivitiSuManagerTest {

    private static final QName SERVICE_NAME = new QName("namespace", "a-service");

    private static final QName INTERFACE_NAME = new QName("namespace", "an-interface");

    private static final String ENDPOINT_NAME = "an-endpoint-name";

    private static final String MY_PROCESS_FILE = "my-process-file.bpmn";

    private static final String MY_2ND_PROCESS_FILE = "another-process-file.bpmn";

    private final URL A_PROCESS_DEFINITION = Thread.currentThread().getContextClassLoader()
            .getResource("orderProcess.bpmn20.xml");

    private static ProcessEngine ACTIVITI_ENGINE;

    @Rule
    final public TemporaryFolder tempFolder = new TemporaryFolder();

    @BeforeClass
    public static void setLogging() throws SecurityException, IOException {
        final InputStream inputStream = ActivitiSuManagerTest.class.getResourceAsStream("/logging.properties");
        assertNotNull("Logging configuration file not found", inputStream);
        try {
            LogManager.getLogManager().readConfiguration(inputStream);
        } finally {
            inputStream.close();
        }
    }

    @BeforeClass
    public static void setActivitiEngine() {
        ACTIVITI_ENGINE = ProcessEngineConfiguration.createStandaloneInMemProcessEngineConfiguration()
                .buildProcessEngine();
    }

    @AfterClass
    public static void stopActivitiEngine() {
        ACTIVITI_ENGINE.close();
    }

    /**
     * <p>
     * Try do deploy a SU containing no process declaration
     * </p>
     * <p>
     * Expected results: an error occurs because no process declaration is included into the JBI descriptor
     * </p>
     */
    @Test
    public void deploy_NoProcessDefinition() throws SecurityException, IllegalArgumentException,
            CDKJBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final ActivitiSuManager activitiSuManager = this.createActivitiSuManager();

        final String suName = "no-process-definition";
        final File suRootPath = this.tempFolder.newFolder(suName);
        this.createSuBpmn(suRootPath, new Element[] {});

        final String result = activitiSuManager.deploy(suName, suRootPath.getAbsolutePath());

        assertTrue("The deployment succeeds: " + result,
                result.contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
        assertTrue("Unexpected error: " + result, result.contains(NoProcessDefinitionDeclarationException.MESSAGE));
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing one process declaration: the process file is set without version.
     * </p>
     * <p>
     * Expected results: an error occurs because the field 'version' is missing into the JBI descriptor.
     * </p>
     */
    @Test
    public void deploy_OneProcessDefinition_MissingVersion() throws SecurityException, IllegalArgumentException,
            CDKJBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final ActivitiSuManager activitiSuManager = this.createActivitiSuManager();

        final String suName = "one-process-definition-missing-version";
        final File suRootPath = this.tempFolder.newFolder(suName);
        this.createSuBpmn(suRootPath, new Element[] { this.createProcessFileExtension(this.createProcessFile(
                suRootPath, MY_PROCESS_FILE, A_PROCESS_DEFINITION)) });

        final String result = activitiSuManager.deploy(suName, suRootPath.getAbsolutePath());

        assertTrue("The deployment succeeds: " + result,
                result.contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
        assertTrue("Unexpected error: " + result, result.contains(String.format(
                IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN, MY_PROCESS_FILE, null)));
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing one process declaration: the process file is set with an empty version.
     * </p>
     * <p>
     * Expected results: an error occurs because the field 'version' is missing into the JBI descriptor.
     * </p>
     */
    @Test
    public void deploy_OneProcessDefinition_EmptyVersion() throws SecurityException, IllegalArgumentException,
            CDKJBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final ActivitiSuManager activitiSuManager = this.createActivitiSuManager();

        final String suName = "one-process-definition-empty-version";
        final File suRootPath = this.tempFolder.newFolder(suName);
        this.createSuBpmn(
                suRootPath,
                new Element[] {
                        this.createProcessFileExtension(this.createProcessFile(suRootPath, MY_PROCESS_FILE,
                                A_PROCESS_DEFINITION)), this.createVersionExtension("") });

        final String result = activitiSuManager.deploy(suName, suRootPath.getAbsolutePath());

        assertTrue("The deployment succeeds: " + result,
                result.contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
        assertTrue("Unexpected error: " + result, result.contains(String.format(
                IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN, MY_PROCESS_FILE, "")));
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing one process declaration: the process file is set with an invalid version.
     * </p>
     * <p>
     * Expected results: an error occurs because the field 'version' is set to an invalid value into the JBI descriptor.
     * </p>
     */
    @Test
    public void deploy_OneProcessDefinition_InvalidVersion() throws SecurityException, IllegalArgumentException,
            CDKJBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final ActivitiSuManager activitiSuManager = this.createActivitiSuManager();

        final String suName = "one-process-definition-invalid-version";
        final File suRootPath = this.tempFolder.newFolder(suName);
        final String version = "invalid";
        this.createSuBpmn(
                suRootPath,
                new Element[] {
                        this.createProcessFileExtension(this.createProcessFile(suRootPath, MY_PROCESS_FILE,
                                A_PROCESS_DEFINITION)), this.createVersionExtension(version) });

        final String result = activitiSuManager.deploy(suName, suRootPath.getAbsolutePath());

        assertTrue("The deployment succeeds: " + result,
                result.contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
        assertTrue("Unexpected error: " + result, result.contains(String.format(
                InvalidVersionDeclaredException.MESSAGE_PATTERN, MY_PROCESS_FILE, version)));
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing one process declaration: the process version is set without its file.
     * </p>
     * <p>
     * Expected results: an error occurs because the field 'process-file' is missing into the JBI descriptor.
     * </p>
     */
    @Test
    public void deploy_OneProcessDefinition_MissingProcessFile() throws SecurityException, IllegalArgumentException,
            CDKJBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final ActivitiSuManager activitiSuManager = this.createActivitiSuManager();

        final String suName = "one-process-definition-missing-processfile";
        final File suRootPath = this.tempFolder.newFolder(suName);
        final String version = "1";
        this.createSuBpmn(suRootPath, new Element[] { this.createVersionExtension(version) });

        final String result = activitiSuManager.deploy(suName, suRootPath.getAbsolutePath());

        assertTrue("The deployment succeeds: " + result,
                result.contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
        assertTrue("Unexpected error: " + result, result.contains(String.format(
                IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN, null, version)));
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing one process declaration: the process version is set and the process file
     * is set to an empty value.
     * </p>
     * <p>
     * Expected results: an error occurs because the field 'process-file' is empty into the JBI descriptor.
     * </p>
     */
    @Test
    public void deploy_OneProcessDefinition_EmptyProcessFile() throws SecurityException, IllegalArgumentException,
            CDKJBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final ActivitiSuManager activitiSuManager = this.createActivitiSuManager();

        final String suName = "one-process-definition-empty-processfile";
        final File suRootPath = this.tempFolder.newFolder(suName);
        final String version = "1";
        this.createSuBpmn(suRootPath,
                new Element[] { this.createProcessFileExtension(""), this.createVersionExtension(version) });

        final String result = activitiSuManager.deploy(suName, suRootPath.getAbsolutePath());

        assertTrue("The deployment succeeds: " + result,
                result.contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
        assertTrue("Unexpected error: " + result, result.contains(String.format(
                IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN, "", version)));
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing one process declaration: the process version is set and the process file
     * is set to an inexisting file.
     * </p>
     * <p>
     * Expected results: an error occurs because the field 'process-file' does not exist.
     * </p>
     */
    @Test
    public void deploy_OneProcessDefinition_UnknownProcessFile() throws SecurityException, IllegalArgumentException,
            CDKJBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final ActivitiSuManager activitiSuManager = this.createActivitiSuManager();

        final String suName = "one-process-definition-unknown-process-file";
        final File suRootPath = this.tempFolder.newFolder(suName);
        this.createSuBpmn(suRootPath,
                new Element[] { this.createProcessFileExtension(MY_PROCESS_FILE), this.createVersionExtension("1") });

        final String result = activitiSuManager.deploy(suName, suRootPath.getAbsolutePath());

        assertTrue("The deployment succeeds: " + result,
                result.contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
        assertTrue("Unexpected error: " + result,
                result.contains(String.format(UnexistingProcessFileException.MESSAGE_PATTERN, new File(suRootPath,
                        MY_PROCESS_FILE).getAbsolutePath())));
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing multiple process declarations where:
     * <ul>
     * <li>a first process declaration is valid,</li>
     * <li>the 2nd process declaration: the process file is set without version.
     * </p>
     * <p>
     * Expected results: As version occurs once, an error about a missing process file occurs because the CDK has read
     * multiple process files as '<code>process_file&lt;i&gt;</code>' but the unique version as '<code>version</code>'.
     * </p>
     */
    @Test
    public void deploy_MultipleProcessDefinition_MissingVersion() throws SecurityException, IllegalArgumentException,
            CDKJBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final ActivitiSuManager activitiSuManager = this.createActivitiSuManager();

        final String suName = "multiple-process-definition-missing-version";
        final File suRootPath = this.tempFolder.newFolder(suName);
        final String version = "1";
        this.createSuBpmn(
                suRootPath,
                new Element[] {
                        this.createProcessFileExtension(this.createProcessFile(suRootPath, MY_PROCESS_FILE,
                                A_PROCESS_DEFINITION)),
                        this.createVersionExtension(version),
                        this.createProcessFileExtension(this.createProcessFile(suRootPath, MY_2ND_PROCESS_FILE,
                                A_PROCESS_DEFINITION)) });

        final String result = activitiSuManager.deploy(suName, suRootPath.getAbsolutePath());

        assertTrue("The deployment succeeds: " + result,
                result.contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
        assertTrue("Unexpected error: " + result, result.contains(String.format(
                IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN, null, version)));
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing multiple process declarations where:
     * <ul>
     * <li>a first process declaration is valid,</li>
     * <li>the 2nd process declaration: the process file is set with an empty version.
     * </p>
     * <p>
     * Expected results: an error occurs because the 2nd field 'version' is empty into the JBI descriptor.
     * </p>
     */
    @Test
    public void deploy_MultipleProcessDefinition_EmptyVersion() throws SecurityException, IllegalArgumentException,
            CDKJBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final ActivitiSuManager activitiSuManager = this.createActivitiSuManager();

        final String suName = "multiple-process-definition-empty-version";
        final File suRootPath = this.tempFolder.newFolder(suName);
        final String version = "1";
        this.createSuBpmn(
                suRootPath,
                new Element[] {
                        this.createProcessFileExtension(this.createProcessFile(suRootPath, MY_PROCESS_FILE,
                                A_PROCESS_DEFINITION)),
                        this.createVersionExtension(version),
                        this.createProcessFileExtension(this.createProcessFile(suRootPath, MY_2ND_PROCESS_FILE,
                                A_PROCESS_DEFINITION)), this.createVersionExtension("") });

        final String result = activitiSuManager.deploy(suName, suRootPath.getAbsolutePath());

        assertTrue("The deployment succeeds: " + result,
                result.contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
        assertTrue("Unexpected error: " + result, result.contains(String.format(
                IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN, MY_2ND_PROCESS_FILE, "")));
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing multiple process declarations where:
     * <ul>
     * <li>a first process declaration is valid,</li>
     * <li>the 2nd process declaration: the process file is set with an invalid version.
     * </p>
     * <p>
     * Expected results: an error occurs because the 2nd field 'version' is invalid into the JBI descriptor.
     * </p>
     */
    @Test
    public void deploy_MultipleProcessDefinition_InvalidVersion() throws SecurityException, IllegalArgumentException,
            CDKJBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final ActivitiSuManager activitiSuManager = this.createActivitiSuManager();

        final String suName = "multiple-process-definition-invalid-version";
        final File suRootPath = this.tempFolder.newFolder(suName);
        final String version1 = "1";
        final String version2 = "invalid";
        this.createSuBpmn(
                suRootPath,
                new Element[] {
                        this.createProcessFileExtension(this.createProcessFile(suRootPath, MY_PROCESS_FILE,
                                A_PROCESS_DEFINITION)),
                        this.createVersionExtension(version1),
                        this.createProcessFileExtension(this.createProcessFile(suRootPath, MY_2ND_PROCESS_FILE,
                                A_PROCESS_DEFINITION)), this.createVersionExtension(version2) });

        final String result = activitiSuManager.deploy(suName, suRootPath.getAbsolutePath());

        assertTrue("The deployment succeeds: " + result,
                result.contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
        assertTrue("Unexpected error: " + result, result.contains(String.format(
                InvalidVersionDeclaredException.MESSAGE_PATTERN, MY_2ND_PROCESS_FILE, version2)));
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing multiple process declarations where:
     * <ul>
     * <li>a first process declaration is valid,</li>
     * <li>the 2nd process declaration: the process version is set without file.
     * </p>
     * <p>
     * Expected results: As process file occurs once, an error about a missing version occurs because the CDK has read
     * multiple process files as '<code>version&lt;i&gt;</code>' but the unique version as '<code>process_file</code>'.
     * </p>
     */
    @Test
    public void deploy_MultipleProcessDefinition_MissingProcessFile() throws SecurityException,
            IllegalArgumentException, CDKJBIDescriptorException, NoSuchFieldException, IllegalAccessException,
            IOException, JBIException, URISyntaxException {

        final ActivitiSuManager activitiSuManager = this.createActivitiSuManager();

        final String suName = "multiple-process-definition-missing-process-file";
        final File suRootPath = this.tempFolder.newFolder(suName);
        final String version1 = "1";
        final String version2 = "2";
        this.createSuBpmn(
                suRootPath,
                new Element[] {
                        this.createProcessFileExtension(this.createProcessFile(suRootPath, MY_PROCESS_FILE,
                                A_PROCESS_DEFINITION)), this.createVersionExtension(version1),
                        this.createVersionExtension(version2) });

        final String result = activitiSuManager.deploy(suName, suRootPath.getAbsolutePath());

        assertTrue("The deployment succeeds: " + result,
                result.contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
        assertTrue("Unexpected error: " + result, result.contains(String.format(
                IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN, MY_PROCESS_FILE, null)));
    }

    /**
     * <p>
     * Try do deploy an invalid SU containing multiple process declarations where:
     * <ul>
     * <li>a first process declaration is valid,</li>
     * <li>the 2nd process declaration: the process version is set with an empty file.
     * </p>
     * <p>
     * Expected results: an error occurs because the 2nd field 'version' is empty into the JBI descriptor.
     * </p>
     */
    @Test
    public void deploy_MultipleProcessDefinition_EmptyProcessFile() throws SecurityException, IllegalArgumentException,
            CDKJBIDescriptorException, NoSuchFieldException, IllegalAccessException, IOException, JBIException,
            URISyntaxException {

        final ActivitiSuManager activitiSuManager = this.createActivitiSuManager();

        final String suName = "multiple-process-definition-empty-process-file";
        final File suRootPath = this.tempFolder.newFolder(suName);
        final String version1 = "1";
        final String version2 = "2";
        this.createSuBpmn(
                suRootPath,
                new Element[] {
                        this.createProcessFileExtension(this.createProcessFile(suRootPath, MY_PROCESS_FILE,
                                A_PROCESS_DEFINITION)), this.createVersionExtension(version1),
                        this.createProcessFileExtension(""), this.createVersionExtension(version2) });

        final String result = activitiSuManager.deploy(suName, suRootPath.getAbsolutePath());

        assertTrue("The deployment succeeds: " + result,
                result.contains("<loc-message>Failed to deploy Service Unit : {1}</loc-message>"));
        assertTrue("Unexpected error: " + result, result.contains(String.format(
                IncoherentProcessDefinitionDeclarationException.MESSAGE_PATTERN, "", version2)));
    }

    private ActivitiSuManager createActivitiSuManager() throws CDKJBIDescriptorException, SecurityException,
            NoSuchFieldException, IllegalArgumentException, IllegalAccessException, JBIException {

        final ActivitiSE component = new ActivitiSE();

        final ServiceEndpoint serviceEndpoint = new ServiceEndpoint() {

            @Override
            public QName getServiceName() {
                return SERVICE_NAME;
            }

            @Override
            public QName[] getInterfaces() {
                return new QName[] { INTERFACE_NAME };
            }

            @Override
            public String getEndpointName() {
                return ENDPOINT_NAME;
            }

            @Override
            public DocumentFragment getAsReference(QName operationName) {
                return null;
            }
        };

        final ComponentContext componentContext = EasyMock.createMock(ComponentContext.class);
        EasyMock.expect(componentContext.getComponentName()).andReturn("petals-se-bpmn").anyTimes();
        EasyMock.expect(componentContext.activateEndpoint(SERVICE_NAME, ENDPOINT_NAME)).andReturn(serviceEndpoint)
                .anyTimes();
        componentContext.deactivateEndpoint(serviceEndpoint);
        EasyMock.expectLastCall().anyTimes();
        EasyMock.replay(componentContext);

        final InputStream defaultJbiDescriptorStream = Thread.currentThread().getContextClassLoader()
                .getResourceAsStream("jbi/jbi.xml");
        assertNotNull("The component JBI descriptor is missing", defaultJbiDescriptorStream);
        final Jbi jbiComponentConfiguration = JBIDescriptorBuilder.buildJavaJBIDescriptor(defaultJbiDescriptorStream);
        final Field jbiComponentCfgField = AbstractComponent.class.getDeclaredField("jbiComponentConfiguration");
        jbiComponentCfgField.setAccessible(true);
        jbiComponentCfgField.set(component, jbiComponentConfiguration);
        final Field loggerField = AbstractComponent.class.getDeclaredField("logger");
        loggerField.setAccessible(true);
        loggerField.set(component, Logger.getLogger(this.getClass().getName()));
        final Field contextField = AbstractComponent.class.getDeclaredField("context");
        contextField.setAccessible(true);
        contextField.set(component, componentContext);
        final Field interceptorsField = AbstractComponent.class.getDeclaredField("interceptors");
        interceptorsField.setAccessible(true);
        interceptorsField.set(component, new HashMap<String, Interceptor>());
        final Field activitiEngineField = ActivitiSE.class.getDeclaredField("activitiEngine");
        activitiEngineField.setAccessible(true);
        activitiEngineField.set(component, ACTIVITI_ENGINE);

        return new ActivitiSuManager(component);
    }

    /**
     * Create a BPMN file into the SU directory tree. The BPMN file copied from an existing one.
     * 
     * @param suRootPath
     *            The SU directory tree
     * @param processFileName
     *            The name of the BPMN file into the SU directory tree
     * @param originalProcessFileURL
     *            The URL of the original BPMN file resource
     * @return The name of the BPMN file into the SU directory tree, ie <code>processFileName</code>.
     */
    private String createProcessFile(final File suRootPath, final String processFileName,
            final URL originalProcessFileURL) throws URISyntaxException, IOException {
        final File processDefinitionInputFile = new File(originalProcessFileURL.toURI());
        final File processDefinitionOutputFile = new File(suRootPath, processFileName);
        final FileInputStream fis = new FileInputStream(processDefinitionInputFile);
        try {
            final FileOutputStream fos = new FileOutputStream(processDefinitionOutputFile);
            try {
                IOUtils.copy(fis, fos);
                return processFileName;
            } finally {
                fos.close();
            }
        } finally {
            fis.close();
        }
    }

    /**
     * Create the component extension {@value ActivitiSEConstants#PROCESS_FILE}
     * 
     * @param processFileExtensionValue
     *            The value to set, or <code>null</code>
     */
    private Element createProcessFileExtension(final String processFileExtensionValue) {
        final Document doc = DocumentBuilders.newDocument();
        final Element processFile = doc.createElementNS("activiti", ActivitiSEConstants.PROCESS_FILE);
        if (processFileExtensionValue != null) {
            processFile.setTextContent(processFileExtensionValue);
        }
        return processFile;
    }

    /**
     * Create the component extension {@value ActivitiSEConstants#VERSION}
     * 
     * @param versionExtensionValue
     *            The value to set, or <code>null</code>
     */
    private Element createVersionExtension(final String versionExtensionValue) {
        final Document doc = DocumentBuilders.newDocument();
        final Element version = doc.createElementNS("activiti", ActivitiSEConstants.VERSION);
        if (versionExtensionValue != null) {
            version.setTextContent(versionExtensionValue);
        }
        return version;
    }

    /**
     * Generate a SU directory tree creating:
     * <ul>
     * <li>the JBI descriptor</li>
     * </ul>
     * 
     * @param suRootPath
     *            The SU root path
     * @param extensions
     *            The component extension to add to the JBI descriptor
     */
    private void createSuBpmn(final File suRootPath, final Element[] extensions) throws CDKJBIDescriptorException,
            IOException {
        final Jbi jbi = new Jbi();
        jbi.setVersion(BigDecimal.valueOf(1));
        final Services services = new Services();
        jbi.setServices(services);
        final Provides provides = new Provides();
        services.getProvides().add(provides);
        provides.setInterfaceName(INTERFACE_NAME);
        provides.setServiceName(SERVICE_NAME);
        provides.setEndpointName(ENDPOINT_NAME);
        provides.getAny().addAll(Arrays.asList(extensions));

        final File metaInfDir = new File(suRootPath, "META-INF");
        assertTrue(metaInfDir.mkdirs());
        final File suJbiDescriptorFile = new File(metaInfDir, "jbi.xml");
        final FileOutputStream fos = new FileOutputStream(suJbiDescriptorFile);
        try {
            JBIDescriptorBuilder.buildXmlJBIdescriptor(jbi, fos);
        } finally {
            fos.close();
        }

    }
}
